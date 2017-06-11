module Main where

import Control.Monad

import System.Console.Haskeline

import Resolver.Parser
import Resolver.Printer
import Resolver.Resolver

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      einput <- getInputLine "resolver> "
      case einput of
        Nothing ->      return ()
        Just "quit" ->  return ()
        Just input -> do
          case parseExpr input of
            (Right expr)  -> outputStrLn $ showExpr (exprToCnf expr)
            (Left err)    -> outputStrLn $ show err
          loop
