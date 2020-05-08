module Main where

import Codegen
import Control.Monad.Trans
import Emit
import qualified LLVM.AST as AST
import Parser
import Syntax
import System.Console.Haskeline
import System.Environment

initModule :: AST.Module
initModule = emptyModule "Kaleidoscope"

parse :: String -> IO (Maybe [Expr])
parse source = do
  let res = parseTopLevel source
  case res of
    Left err -> print err >> return Nothing
    Right ex -> do
      print ex
      return $ Just ex

process :: AST.Module -> String -> IO (Maybe AST.Module)
process modo source = do
  ex <- liftIO $ parse source
  case ex of
    Nothing -> return Nothing
    Just ex -> do
      ast <- codegen modo ex
      return $ Just ast

processFile :: String -> IO (Maybe AST.Module)
processFile fname = readFile fname >>= process initModule

repl :: IO ()
repl = runInputT defaultSettings (loop initModule)
  where
    loop mod = do
      minput <- getInputLine "ready> "
      case minput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> do
          modn <- liftIO $ process mod input
          case modn of
            Just modn -> loop modn
            Nothing -> loop mod

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl
    [fname] -> processFile fname >> return ()
