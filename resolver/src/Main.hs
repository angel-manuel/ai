module Main where

import Control.Arrow
import Data.List

data LogicalExpression =
  Literal String
  | T
  | F
  | Negation LogicalExpression
  | And [LogicalExpression]
  | Or [LogicalExpression]
  | If LogicalExpression LogicalExpression
  | Iff LogicalExpression LogicalExpression
  deriving Show;

precendence :: LogicalExpression -> Int
precendence T = 0
precendence F = 0
precendence (Literal _) = 0
precendence (Negation _) = 1
precendence (And _) = 2
precendence (Or _) = 2
precendence (If _ _) = 3
precendence (Iff _ _) = 3

repr_expr :: Int -> LogicalExpression -> String
repr_expr prec expr =
  if prec <= my_prec then parenthesize composed_expr else composed_expr
    where
      my_prec = precendence expr
      parenthesize s = "(" ++ s ++ ")"
      composed_expr = compose_expr my_prec expr

compose_expr :: Int -> LogicalExpression -> String
compose_expr _ (Literal s) = s
compose_expr _ T = "T"
compose_expr _ F = "F"
compose_expr _ (And []) = "[^]"
compose_expr _ (Or []) = "[v]"
compose_expr prec (Negation e) = "¬" ++ repr_expr prec e
compose_expr prec (And (he:le)) = (repr_expr prec he) ++ (concat $ map (\e ->  "^" ++ (repr_expr prec e)) le)
compose_expr prec (Or (he:le)) = (repr_expr prec he) ++ (concat $ map (\e ->  "v" ++ (repr_expr prec e)) le)
compose_expr prec (If e1 e2) = (repr_expr prec e1) ++ "=>" ++ (repr_expr prec e2)
compose_expr prec (Iff e1 e2) = (repr_expr prec e1) ++ "<=>" ++ (repr_expr prec e2)

show_expr :: LogicalExpression -> String
show_expr expr = compose_expr (precendence expr) expr

expr_to_and_arr (And a_exprs) = a_exprs
expr_to_and_arr e = [e]

expr_to_or_arr (Or a_exprs) = a_exprs
expr_to_or_arr e = [e]

distribute_or :: [[[LogicalExpression]]] -> [[LogicalExpression]]
distribute_or [] = []
distribute_or [a_exprs] = a_exprs
distribute_or (a_exprs:o_exprs) = map (\o_r_expr -> map (\o_expr -> Or $ concat $ map expr_to_or_arr $ o_expr ++ o_r_expr) a_exprs) (distribute_or o_exprs)

expr_to_cnf :: LogicalExpression -> LogicalExpression
expr_to_cnf expr@(Literal _) = expr
expr_to_cnf expr@(Negation (Literal _)) = expr
expr_to_cnf (Negation (Negation expr)) = expr_to_cnf expr
expr_to_cnf (Negation (And exprs)) = expr_to_cnf $ Or $ map Negation exprs
expr_to_cnf (Negation (Or exprs)) = expr_to_cnf $ And $ map Negation exprs
expr_to_cnf (Negation T) = F
expr_to_cnf (Negation F) = T
expr_to_cnf (Negation e) = expr_to_cnf $ Negation $ expr_to_cnf e
expr_to_cnf (And []) = F
expr_to_cnf (Or []) = T
expr_to_cnf (And [expr]) = expr_to_cnf expr
expr_to_cnf (Or [expr]) = expr_to_cnf expr
expr_to_cnf (And exprs) = And $ concat (map (expr_to_and_arr.expr_to_cnf) exprs)
expr_to_cnf (Or exprs) = And $ concat $ distribute_or (map ((map expr_to_or_arr).expr_to_and_arr.expr_to_cnf) exprs)
expr_to_cnf (If e1 e2) = expr_to_cnf $ Or [Negation e1, e2]
expr_to_cnf (Iff e1 e2) = expr_to_cnf $ And [If e1 e2, If e2 e1]
expr_to_cnf e = e

main :: IO ()
main = do
  putStrLn "Resolver"
  putStrLn $ (show_expr e1) ++ " ===> " ++ (show_expr (expr_to_cnf e1))
  putStrLn $ (show_expr e2) ++ " ===> " ++ (show_expr (expr_to_cnf e2))
  putStrLn $ (show_expr e3) ++ " ===> " ++ (show_expr (expr_to_cnf e3))
    where
      e1 = Negation $ Or [(And [Negation (Literal "A"), If (Literal "B") (Literal "C")]), Literal "C"]
      e2 = Iff (And [Literal "A", Literal "B", Literal "C"]) (Or [Literal "D", Negation (Literal "E")])
      e3 = Iff (Literal "A") (Literal "B")
