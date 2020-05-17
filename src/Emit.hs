module Emit where

import Codegen
import Control.Monad (forM_)
import Control.Monad.State (gets)
import Data.ByteString.UTF8 (toString)
--import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Map as Map
--import LLVM.Pretty (ppllvm)

import JIT
import LLVM.AST (Definition, Module, moduleDefinitions)
import LLVM.AST.AddrSpace (AddrSpace (AddrSpace))
import qualified LLVM.AST.Constant as Constant
import LLVM.AST.Float (SomeFloat (Double))
import LLVM.AST.FloatingPointPredicate (FloatingPointPredicate (ONE))
import qualified LLVM.AST.Name as Name
import LLVM.AST.Operand (Operand (ConstantOperand))
import LLVM.AST.Type (Type(FunctionType, PointerType))
import LLVM.AST.Typed (typeOf)
import LLVM.Context (withContext)
import LLVM.Module (moduleLLVMAssembly, withModuleFromAST)
import Syntax

one = ConstantOperand $ Constant.Float (Double 1.0)

zero = ConstantOperand $ Constant.Float (Double 0.0)

false = zero

true = one

toSig :: [String] -> [(Type, Name.Name)]
toSig = map (\x -> (double, Name.mkName x))

binops =
  Map.fromList
    [ ("+", fadd),
      ("-", fsub),
      ("*", fmul),
      ("/", fdiv),
      ("<", lt)
    ]

codegenTop :: Expr -> LLVM ()
codegenTop (Syntax.Function name args body) = do
  let bls = createBlocks $ execCodegen $ do
        b <- addBlock entryBlockName
        _ <- setBlock b
        forM_ args $ \a -> do
          var <- alloca double
          store var $ local $ Name.mkName a
          assign a var
        cgen body >>= ret
  define double name fnargs bls
  where
    fnargs = toSig args
codegenTop (Extern name args) =
  external double name fnargs
  where
    fnargs = toSig args
codegenTop (UnaryDef name args body) =
  codegenTop $ Function ("unary" ++ name) args body
codegenTop (BinaryDef name args body) =
  codegenTop $ Function ("binary" ++ name) args body
codegenTop expr = do
  let blks = createBlocks $ execCodegen $ do
        b <- addBlock entryBlockName
        _ <- setBlock b
        cgen expr >>= ret
  define double "main" [] blks

cgen :: Expr -> Codegen Operand
cgen (UnaryOp op a) =
  cgen $ Syntax.Call ("unary" ++ op) [a]
cgen (BinaryOp "=" (Var var) val) = do
  a <- getvar var
  cval <- cgen val
  store a cval
  return cval
cgen (BinaryOp op a b) =
  case Map.lookup op binops of
    Just f -> do
      ca <- cgen a
      cb <- cgen b
      f ca cb
    Nothing -> cgen (Call ("binary" ++ op) [a, b])
cgen (Var x) = getvar x >>= load
cgen (Float n) = return $ ConstantOperand $ Constant.Float (Double n)
cgen (Syntax.Call fn args) = do
  largs <- mapM cgen args
  call (externf fnType fnName) largs
  where
    fnName = Name.mkName fn
    fnType = PointerType (FunctionType double (replicate (length args) double) False) (AddrSpace 0) --lookupFnType defs fnName
cgen (Syntax.If cond tr fl) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"
  -- %entry
  cond <- cgen cond
  test <- fcmp ONE false cond
  cbr test ifthen ifelse -- Branch based on the condition
    -- if.then
  setBlock ifthen
  trval <- cgen tr -- Generate code for the true branch
  br ifexit -- Branch to the merge block
  ifthen <- getBlock
  -- if.else
  setBlock ifelse
  flval <- cgen fl -- Generate code for the false branch
  br ifexit -- Branch to the merge block
  ifelse <- getBlock
  -- if.exit
  setBlock ifexit
  phi double [(trval, ifthen), (flval, ifelse)]
cgen (Syntax.For ivar start cond step body) = do
  forloop <- addBlock "for.loop"
  forexit <- addBlock "for.exit"
  -- %entry
  i <- alloca double
  istart <- cgen start -- Generate loop variable initial value
  stepval <- cgen step -- Generate loop variable step
  store i istart -- Store the loop variable initial value
  assign ivar i -- Assign loop variable to the variable name
  br forloop -- Branch to the loop body block
    -- for.loop
  setBlock forloop
  cgen body -- Generate the loop body
  ival <- load i -- Load the current loop iteration
  inext <- fadd ival stepval -- Increment loop variable
  store i inext
  cond <- cgen cond -- Generate the loop condition
  test <- fcmp ONE false cond -- Test if the loop condition is true
  cbr test forloop forexit -- Generate the loop condition
    -- for.exit
  setBlock forexit
  return zero

codegen :: Module -> [Expr] -> IO Module
codegen m fns =
  withContext $ \context -> do
    print oldast
    newast <- runJIT oldast
    --TIO.putStrLn $ ppllvm newast
    llstr <- withModuleFromAST context newast moduleLLVMAssembly
    putStrLn $ toString llstr
    return newast
  where
    modn = mapM codegenTop fns
    oldast = runLLVM m modn
