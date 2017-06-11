module Resolver.Printer (
  showExpr
) where

import Resolver.Resolver (LogicalExpression(..))

precendence :: LogicalExpression -> Int
precendence T = 0
precendence F = 0
precendence (Literal _) = 0
precendence (Negation _) = 1
precendence (And _) = 2
precendence (Or _) = 2
precendence (If _ _) = 3
precendence (Iff _ _) = 4

reprExpr :: Int -> LogicalExpression -> String
reprExpr prec expr =
  if prec <= my_prec then parenthesize composedExpr else composedExpr
    where
      my_prec = precendence expr
      parenthesize s = "(" ++ s ++ ")"
      composedExpr = composeExpr my_prec expr

composeExpr :: Int -> LogicalExpression -> String
composeExpr _ (Literal s) = s
composeExpr _ T = "T"
composeExpr _ F = "F"
composeExpr _ (And []) = "[^]"
composeExpr _ (Or []) = "[|]"
composeExpr prec (Negation e) = "¬" ++ reprExpr prec e
composeExpr prec (And (he:le)) = (reprExpr prec he) ++ (concat $ map (\e ->  "^" ++ (reprExpr prec e)) le)
composeExpr prec (Or (he:le)) = (reprExpr prec he) ++ (concat $ map (\e ->  "|" ++ (reprExpr prec e)) le)
composeExpr prec (If e1 e2) = (reprExpr prec e1) ++ "=>" ++ (reprExpr prec e2)
composeExpr prec (Iff e1 e2) = (reprExpr prec e1) ++ "<=>" ++ (reprExpr prec e2)

showExpr :: LogicalExpression -> String
showExpr expr = composeExpr (precendence expr) expr
