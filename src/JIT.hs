module JIT where

import Data.ByteString.UTF8 (toString)
import Foreign.Ptr (FunPtr, castFunPtr)
import LLVM.AST (Module)
import LLVM.AST.Name (mkName)
import LLVM.Context (Context, withContext)
import LLVM.ExecutionEngine
  ( MCJIT,
    getFunction,
    withMCJIT,
    withModuleInEngine,
  )
import LLVM.Module (moduleAST, moduleLLVMAssembly, withModuleFromAST)
import LLVM.PassManager
  ( PassSetSpec,
    defaultCuratedPassSetSpec,
    optLevel,
    runPassManager,
    withPassManager,
  )

foreign import ccall "dynamic" haskFun :: FunPtr (IO Double) -> IO Double

runJIT :: Module -> IO Module
runJIT md =
  withContext $ \context -> 
  jit context $ \executionEngine ->
  withModuleFromAST context md $ \m ->
    withPassManager passes $ \pm -> do
      _ <- runPassManager pm m
      optmod <- moduleAST m
      s <- moduleLLVMAssembly m
      putStrLn $ toString s

      withModuleInEngine executionEngine m $ \ee -> do
        mainfn <- getFunction ee (mkName "main")
        case mainfn of
          Just fn -> do
            res <- run fn
            putStrLn $ "Evaluated to: " ++ show res
          Nothing -> return ()
      return optmod

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec {optLevel = Just 3}

jit :: Context -> (MCJIT -> IO a) -> IO a
jit c = withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 2
    model = Nothing
    ptrelim = Nothing
    fastins = Nothing

run :: FunPtr a -> IO Double
run fn = haskFun (castFunPtr fn :: FunPtr (IO Double))
