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
import qualified LLVM.AST.Constant as Constant
import LLVM.AST.Float (SomeFloat (Double))
import LLVM.AST.FloatingPointPredicate (FloatingPointPredicate (ONE))
import qualified LLVM.AST.Name as Name
import LLVM.AST.Operand (Operand (ConstantOperand))
import LLVM.AST.Type (Type)
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
  defs <- gets moduleDefinitions
  let bls = createBlocks $ execCodegen $ do
        b <- addBlock entryBlockName
        _ <- setBlock b
        forM_ args $ \a -> do
          var <- alloca double
          store var $ local $ Name.mkName a
          assign a var
        cgen defs body >>= ret
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
  defs <- gets moduleDefinitions
  let blks = createBlocks $ execCodegen $ do
        b <- addBlock entryBlockName
        _ <- setBlock b
        cgen defs expr >>= ret
  define double "main" [] blks

cgen :: [Definition] -> Expr -> Codegen Operand
cgen defs (UnaryOp op a) =
  cgen defs $ Syntax.Call ("unary" ++ op) [a]
cgen defs (BinaryOp "=" (Var var) val) = do
  a <- getvar var
  cval <- cgen defs val
  store a cval
  return cval
cgen defs (BinaryOp op a b) =
  case Map.lookup op binops of
    Just f -> do
      ca <- cgen defs a
      cb <- cgen defs b
      f ca cb
    Nothing -> cgen defs (Call ("binary" ++ op) [a,b])
cgen _ (Var x) = getvar x >>= load
cgen _ (Float n) = return $ ConstantOperand $ Constant.Float (Double n)
cgen defs (Syntax.Call fn args) = do
  largs <- mapM (cgen defs) args
  call (externf fnType fnName) largs
  where
    fnName = Name.mkName fn
    fnType = lookupFnType defs fnName
cgen defs (Syntax.If cond tr fl) = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"
  -- %entry
  cond <- cgen defs cond
  test <- fcmp ONE false cond
  cbr test ifthen ifelse -- Branch based on the condition
    -- if.then
  setBlock ifthen
  trval <- cgen defs tr -- Generate code for the true branch
  br ifexit -- Branch to the merge block
  ifthen <- getBlock
  -- if.else
  setBlock ifelse
  flval <- cgen defs fl -- Generate code for the false branch
  br ifexit -- Branch to the merge block
  ifelse <- getBlock
  -- if.exit
  setBlock ifexit
  phi double [(trval, ifthen), (flval, ifelse)]
cgen defs (Syntax.For ivar start cond step body) = do
  forloop <- addBlock "for.loop"
  forexit <- addBlock "for.exit"
  -- %entry
  i <- alloca double
  istart <- cgen defs start -- Generate loop variable initial value
  stepval <- cgen defs step -- Generate loop variable step
  store i istart -- Store the loop variable initial value
  assign ivar i -- Assign loop variable to the variable name
  br forloop -- Branch to the loop body block
    -- for.loop
  setBlock forloop
  cgen defs body -- Generate the loop body
  ival <- load i -- Load the current loop iteration
  inext <- fadd ival stepval -- Increment loop variable
  store i inext
  cond <- cgen defs cond -- Generate the loop condition
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
