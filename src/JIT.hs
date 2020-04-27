module JIT where

import Control.Monad.Except (runExceptT)
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (toString)
import LLVM.AST (Module)
import LLVM.Context (Context, withContext)
import LLVM.Module (moduleLLVMAssembly, withModuleFromAST, moduleAST)
import LLVM.PassManager (PassSetSpec, defaultCuratedPassSetSpec, optLevel, withPassManager, runPassManager)

runJIT :: Module -> IO (Module)
runJIT mod = do
  withContext $ \context -> do
    withModuleFromAST context mod $ \m -> do
      withPassManager passes $ \pm -> do
        runPassManager pm m
        optmod <- moduleAST m
        s <- moduleLLVMAssembly m
        putStrLn $ toString s
        return optmod

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec {optLevel = Just 3}
