{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Codegen where

import Control.Monad.State (MonadState, State, execState, gets, modify)
import Data.ByteString.Short (toShort)
import Data.ByteString.UTF8 (fromString)
import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Map as Map
import LLVM.AST
  ( Definition (GlobalDefinition),
    Module,
    defaultModule,
    functionDefaults,
    moduleDefinitions,
    moduleName,
    noFastMathFlags,
  )
import LLVM.AST.AddrSpace (AddrSpace (AddrSpace))
import LLVM.AST.Attribute (ParameterAttribute)
import LLVM.AST.CallingConvention (CallingConvention (C))
import LLVM.AST.Constant (Constant (GlobalReference))
import LLVM.AST.FloatingPointPredicate (FloatingPointPredicate (ULT))
import LLVM.AST.Global
  ( BasicBlock (BasicBlock),
    Global (Function),
    Parameter (Parameter),
    basicBlocks,
    linkage,
    name,
    parameters,
    returnType,
  )
import LLVM.AST.Instruction
  ( Instruction
      ( Alloca,
        Call,
        FAdd,
        FCmp,
        FDiv,
        FMul,
        FSub,
        Load,
        Phi,
        Store,
        UIToFP
      ),
    Named ((:=), Do),
    Terminator (Br, CondBr, Ret),
  )
import LLVM.AST.Linkage (Linkage (External))
import LLVM.AST.Name (Name (UnName), mkName)
import LLVM.AST.Operand (Operand (ConstantOperand, LocalReference))
import LLVM.AST.Type
  ( FloatingPointType (DoubleFP),
    Type (FloatingPointType, PointerType),
  )
import LLVM.AST.Typed (typeOf)

double :: Type
double = FloatingPointType DoubleFP

type SymbolTable = [(String, Operand)]

type Names = Map.Map String Int

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm, Map.insert nm 1 ns)
    Just ix -> (nm ++ show ix, Map.insert nm (ix + 1) ns)

data BlockState
  = BlockState
      { idx :: Int, -- Block index
        stack :: [Named Instruction], -- Stack of instructions
        term :: Maybe (Named Terminator) -- Block terminator
      }
  deriving (Show)

data CodegenState
  = CodegenState
      { currentBlock :: Name, -- Name of the active block to append to
        blocks :: Map.Map Name BlockState, -- Blocks for function
        symtab :: SymbolTable, -- Function scope symbol table
        blockCount :: Int, -- Count of basic blocks
        count :: Word, -- Count of unamed instructions
        names :: Names -- Name supply
      }
  deriving (Show)

newtype Codegen a = Codegen {runCodegen :: State CodegenState a}
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

newtype LLVM a = LLVM (State Module a)
  deriving (Functor, Applicative, Monad, MonadState Module)

runLLVM :: Module -> LLVM a -> Module
runLLVM m (LLVM mn) = execState mn m

emptyModule :: String -> Module
emptyModule label = defaultModule {moduleName = toShort $ fromString label}

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s {moduleDefinitions = defs ++ [d]}

define :: Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body =
  addDefn
    $ GlobalDefinition
    $ functionDefaults
      { name = mkName label,
        parameters = ([Parameter ty nm [] | (ty, nm) <- argtys], False),
        returnType = retty,
        basicBlocks = body
      }

external :: Type -> String -> [(Type, Name)] -> LLVM ()
external retty label argtys =
  addDefn
    $ GlobalDefinition
    $ functionDefaults
      { name = mkName label,
        linkage = External,
        parameters = ([Parameter ty nm [] | (ty, nm) <- argtys], False),
        returnType = retty,
        basicBlocks = []
      }

entry :: Codegen Name
entry = gets currentBlock

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

addBlock :: String -> Codegen Name
addBlock bname = do
  bls <- gets blocks
  ix <- gets blockCount
  nms <- gets names
  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms
      sqname = mkName qname
  modify $ \s ->
    s
      { blocks = Map.insert sqname new bls,
        blockCount = ix + 1,
        names = supply
      }
  return sqname

getBlock :: Codegen Name
getBlock = gets currentBlock

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, BlockState _ s t) = BasicBlock l (reverse s) (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ show l

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s {blocks = Map.insert active new (blocks s)}

setBlock :: Name -> Codegen Name
setBlock bname = do
  modify $ \s -> s {currentBlock = bname}
  return bname

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show c

fresh :: Codegen Word
fresh = do
  i <- gets Codegen.count
  modify $ \s -> s {Codegen.count = 1 + i}
  return $ i + 1

local :: Name -> Operand
local = LocalReference double

externf :: Type -> Name -> Operand
externf ty n = ConstantOperand $ GlobalReference ty n

lookupFnType :: [Definition] -> Name -> Type
lookupFnType defs nm =
  case fnDefByName of
    [] -> error $ "Undefined function: " ++ show nm
    [fn] -> PointerType (typeOf fn) (AddrSpace 0)
    _ -> error $ "Ambiguous function name: " ++ show nm
  where
    globalDefs = [g | GlobalDefinition g <- defs]
    fnDefByName = [f | f@Function {name = nm'} <- globalDefs, nm' == nm]

assign :: String -> Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s {symtab = (var, x) : lcls}

getvar :: String -> Codegen Operand
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var

instr :: Instruction -> Codegen Operand
instr ins = do
  n <- fresh
  let ref = UnName n
  blk <- current
  let i = stack blk
  modifyBlock (blk {stack = (ref := ins) : i})
  return $ local ref

unnminstr :: Instruction -> Codegen ()
unnminstr ins = do
  blk <- current
  let i = stack blk
  modifyBlock (blk {stack = Do ins : i})

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk {term = Just trm})
  return trm

lt :: Operand -> Operand -> Codegen Operand
lt a b = do
  test <- fcmp ULT a b
  uitofp test

fadd :: Operand -> Operand -> Codegen Operand
fadd a b = instr $ FAdd noFastMathFlags a b []

fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instr $ FSub noFastMathFlags a b []

fmul :: Operand -> Operand -> Codegen Operand
fmul a b = instr $ FMul noFastMathFlags a b []

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instr $ FDiv noFastMathFlags a b []

fcmp :: FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
fcmp cond a b = instr $ FCmp cond a b []

uitofp :: Operand -> Codegen Operand
uitofp a = instr $ UIToFP a double []

br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

phi :: Type -> [(Operand, Name)] -> Codegen Operand
phi ty incoming = instr $ Phi ty incoming []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

toArgs :: [Operand] -> [(Operand, [ParameterAttribute])]
toArgs = map (,[])

call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ Call Nothing C [] (Right fn) (toArgs args) [] []

alloca :: Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen ()
store ptr val = unnminstr $ Store False ptr val Nothing 0 []

load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

entryBlockName :: String
entryBlockName = "entry"

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (mkName entryBlockName) Map.empty [] 1 0 Map.empty

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen
