module Main where

import Codegen
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import Emit
import LLVM.AST (Module)
import Parser
import Syntax
import System.Console.Haskeline (defaultSettings, getInputLine, outputStrLn, runInputT)
import System.Environment (getArgs)

initModule :: Module
initModule = emptyModule "Kaleidoscope"

parse :: String -> IO (Maybe [Expr])
parse source = do
  let res = parseTopLevel source
  case res of
    Left err -> print err >> return Nothing
    Right ex -> do
      print ex
      return $ Just ex

process :: Module -> String -> IO (Maybe Module)
process modo source = do
  ex <- liftIO $ parse source
  case ex of
    Nothing -> return Nothing
    Just ex -> do
      ast <- codegen modo ex
      return $ Just ast

processFile :: String -> IO (Maybe Module)
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
    [fname] -> void (processFile fname)
