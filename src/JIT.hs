module JIT where

import Control.Monad.Except (runExceptT)
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (toString)
import LLVM.AST (Module)
import LLVM.Context (Context, withContext)
import LLVM.Module (moduleLLVMAssembly, withModuleFromAST)

runJIT :: Module -> IO ()
runJIT mod = do
  withContext $ \context -> do
    withModuleFromAST context mod $ \m -> do
      s <- moduleLLVMAssembly m
      putStrLn $ toString s
