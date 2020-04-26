module Emit where

import Codegen
import Control.Monad (forM)
import Control.Monad.Except (runExceptT)
import Control.Monad.State (gets)
import Data.ByteString.UTF8 (toString)
--import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Map as Map
import LLVM.AST (Definition, Module, moduleDefinitions)
import qualified LLVM.AST.Name as Name
import LLVM.AST.Operand (Operand(ConstantOperand))
import LLVM.AST.Type (Type)
import qualified LLVM.AST.Constant as Constant
import LLVM.AST.Float (SomeFloat (Double))
import LLVM.Context (withContext)
import LLVM.Module (moduleLLVMAssembly, withModuleFromAST)
--import LLVM.Pretty (ppllvm)
import Syntax

toSig :: [String] -> [(Type, Name.Name)]
toSig = map (\x -> (double, Name.mkName x))

-- lt :: Operand -> Operand -> Codegen AST.Operand
-- lt a b = do
--   test <- fcmp ULT a b
--   uitofp double test

binops =
  Map.fromList
    [ ("+", fadd),
      ("-", fsub),
      ("*", fmul),
      ("/", fdiv)
      --  ("<", lt)
    ]

codegenTop :: Expr -> LLVM ()
codegenTop (Syntax.Function name args body) = do
  defs <- gets moduleDefinitions
  let bls = createBlocks $ execCodegen $ do
        entry <- addBlock entryBlockName
        setBlock entry
        forM args $ \a -> do
          var <- alloca double
          store var $ local $ Name.mkName a
          assign a var
        cgen defs body >>= ret
  define double name fnargs bls
  where
    fnargs = toSig args
codegenTop (Extern name args) = do
  external double name fnargs
  where
    fnargs = toSig args
codegenTop exp = do
  defs <- gets moduleDefinitions
  let blks = createBlocks $ execCodegen $ do
        entry <- addBlock entryBlockName
        setBlock entry
        cgen defs exp >>= ret
  define double "main" [] blks

cgen :: [Definition] -> Expr -> Codegen Operand
cgen defs (UnaryOp op a) = do
  cgen defs $ Syntax.Call ("unary" ++ op) [a]
cgen defs (BinaryOp "=" (Var var) val) = do
  a <- getvar var
  cval <- cgen defs val
  store a cval
  return cval
cgen defs (BinaryOp op a b) = do
  case Map.lookup op binops of
    Just f -> do
      ca <- cgen defs a
      cb <- cgen defs b
      f ca cb
    Nothing -> error "No such operator"
cgen _ (Var x) = getvar x >>= load
cgen _ (Float n) = return $ ConstantOperand $ Constant.Float (Double n)
cgen defs (Syntax.Call fn args) = do
  largs <- mapM (cgen defs) args
  call (externf fnType fnName) largs
  where
    fnName = Name.mkName fn
    fnType = lookupFnType defs fnName

codegen :: Module -> [Expr] -> IO Module
codegen mod fns =
  withContext $ \context -> do
    putStrLn $ show newast
    --TIO.putStrLn $ ppllvm newast
    llstr <- withModuleFromAST context newast moduleLLVMAssembly
    putStrLn $ toString llstr
    return newast
  where
    modn = mapM codegenTop fns
    newast = runLLVM mod modn