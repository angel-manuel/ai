module Main where

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
              outputStrLn $ "KB = " ++ (showExpr $ explainKB new_kb)
              loop new_kb
            (Right (Sat expr))  -> do
              outputStrLn $ show $ satKB expr kb
              loop kb
            (Right (Ask expr))  -> do
              outputStrLn $ show $ askKB expr kb
              loop kb
            (Left err)          -> do
              outputStrLn $ show err
              loop kb
