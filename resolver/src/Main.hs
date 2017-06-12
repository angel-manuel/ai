module Main where

import Control.Monad
import Control.Monad.State.Lazy

import System.Console.Haskeline

import Resolver.Parser
import Resolver.Printer
import Resolver.Resolver

main :: IO ()
main = runInputT defaultSettings $ loop emptyKB
  where
    loop kb = do
      einput <- getInputLine "resolver> "
      case einput of
        Nothing     -> return ()
        Just "q"    -> return ()
        Just "quit" -> return ()
        Just input  -> do
          case parseCommand input of
            (Right (Tell expr)) -> do
              let new_kb = tellKB expr kb
              outputStrLn $ showExpr $ explainKB new_kb
              loop new_kb
            (Right (Ask expr))  -> do
              outputStrLn $ show $ askKB expr kb
              loop kb
            (Left err)          -> do
              outputStrLn $ show err
              loop kb
