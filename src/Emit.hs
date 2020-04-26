module Emit where

import Codegen
import Control.Monad (forM)
import Control.Monad.Except (runExceptT)
import Data.ByteString.UTF8 (toString)
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Map as Map
import LLVM.AST (Module)
import qualified LLVM.AST.Name as Name
import LLVM.AST.Operand (Operand(ConstantOperand))
import LLVM.AST.Type (Type)
import qualified LLVM.AST.Constant as Constant
import LLVM.AST.Float (SomeFloat (Double))
import LLVM.Context (withContext)
import LLVM.Module (moduleLLVMAssembly, withModuleFromAST)
import LLVM.Pretty (ppllvm)
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
  define double name fnargs bls
  where
    fnargs = toSig args
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM args $ \a -> do
        var <- alloca double
        store var $ local $ Name.mkName a
        assign a var
      cgen body >>= ret
codegenTop (Extern name args) = do
  external double name fnargs
  where
    fnargs = toSig args
codegenTop exp = do
  define double "main" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen exp >>= ret

cgen :: Expr -> Codegen Operand
cgen (UnaryOp op a) = do
  cgen $ Syntax.Call ("unary" ++ op) [a]
cgen (BinaryOp "=" (Var var) val) = do
  a <- getvar var
  cval <- cgen val
  store a cval
  return cval
cgen (BinaryOp op a b) = do
  case Map.lookup op binops of
    Just f -> do
      ca <- cgen a
      cb <- cgen b
      f ca cb
    Nothing -> error "No such operator"
cgen (Var x) = getvar x >>= load
cgen (Float n) = return $ ConstantOperand $ Constant.Float (Double n)
cgen (Syntax.Call fn args) = do
  largs <- mapM cgen args
  call (externf $ Name.mkName fn) largs

codegen :: Module -> [Expr] -> IO Module
codegen mod fns =
  withContext $ \context -> do
    TIO.putStrLn $ ppllvm newast
    putStrLn $ show newast
    llstr <- withModuleFromAST context newast moduleLLVMAssembly
    putStrLn $ toString llstr
    return newast
  where
    modn = mapM codegenTop fns
    newast = runLLVM mod modn
