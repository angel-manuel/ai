module Resolver.Resolver (
    exprToCnf
  , exprToCnfArr
  , LogicalExpression(..)
) where

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

data LogicalAtom =
  Atom String
  | Negated String

negateAtom :: LogicalAtom -> LogicalAtom
negateAtom (Atom name)    = Negated name
negateAtom (Negated name) = Atom name

exprToCnf :: LogicalExpression -> LogicalExpression
exprToCnf expr = And $ map (\o_exprs -> Or $ map atomToExpr o_exprs) $ exprToCnfArr expr
  where
    atomToExpr (Atom name)    = Literal name
    atomToExpr (Negated name) = Negation (Literal name)

distributeOr :: [[[LogicalAtom]]] -> [[LogicalAtom]]
distributeOr [] = []
distributeOr [ao_exprs] = ao_exprs
distributeOr (ao_expr:oao_expr) = concat $ map (\o_expr -> map (o_expr++) rest_ao_expr) ao_expr
  where
    rest_ao_expr = distributeOr oao_expr

exprToCnfArr :: LogicalExpression -> [[LogicalAtom]]
exprToCnfArr (Literal name) = [[Atom name]]
exprToCnfArr (Negation (Literal name)) = [[Negated name]]
exprToCnfArr (Negation (Negation expr)) = exprToCnfArr expr
exprToCnfArr (Negation (And exprs)) = exprToCnfArr $ Or $ map Negation exprs
exprToCnfArr (Negation (Or exprs)) = exprToCnfArr $ And $ map Negation exprs
exprToCnfArr (Negation T) = exprToCnfArr F
exprToCnfArr (Negation F) = exprToCnfArr T
exprToCnfArr (Negation e) = exprToCnfArr $ Negation $ exprToCnf e
exprToCnfArr (And []) = []
exprToCnfArr (Or []) = [[]]
exprToCnfArr (And [expr]) = exprToCnfArr expr
exprToCnfArr (Or [expr]) = exprToCnfArr expr
exprToCnfArr (And exprs) = concat $ map exprToCnfArr exprs
exprToCnfArr (Or exprs) = distributeOr (map exprToCnfArr exprs)
exprToCnfArr (If e1 e2) = exprToCnfArr $ Or [Negation e1, e2]
exprToCnfArr (Iff e1 e2) = exprToCnfArr $ And [If e1 e2, If e2 e1]
exprToCnfArr T = [[]]
exprToCnfArr F = []
