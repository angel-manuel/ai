module Resolver.Resolver (
    exprToCnf
  , exprToCnfArr
  , exprToCnfSet
  , LogicalExpression(..)
) where

import Control.Monad.State.Lazy
import Data.List
import qualified Data.Set as Set

data LogicalExpression =
  Literal String
  | T
  | F
  | Negation LogicalExpression
  | And [LogicalExpression]
  | Or [LogicalExpression]
  | If LogicalExpression LogicalExpression
  | Iff LogicalExpression LogicalExpression
  deriving (Show, Eq);

data LogicalAtom =
  Atom String
  | Negated String
  deriving (Show, Eq, Ord);

negateAtom :: LogicalAtom -> LogicalAtom
negateAtom (Atom name)    = Negated name
negateAtom (Negated name) = Atom name

exprToCnf :: LogicalExpression -> LogicalExpression
exprToCnf expr = And $ fmap (\o_exprs -> Or $ fmap atomToExpr o_exprs) $ exprToCnfArr expr
  where
    atomToExpr (Atom name)    = Literal name
    atomToExpr (Negated name) = Negation (Literal name)

distributeOr :: [Set.Set (Set.Set LogicalAtom)] -> Set.Set (Set.Set LogicalAtom)
distributeOr [] = Set.empty
distributeOr [ao_exprs] = ao_exprs
distributeOr (ao_expr:oao_expr) = Set.unions $ Set.toList $ Set.map (\o_expr -> Set.map (Set.union o_expr) rest_ao_expr) ao_expr
  where
    rest_ao_expr = distributeOr oao_expr

exprToCnfArr :: LogicalExpression -> [[LogicalAtom]]
exprToCnfArr expr = map Set.toList $ Set.toList $ exprToCnfSet expr

exprToCnfSet :: LogicalExpression -> Set.Set (Set.Set LogicalAtom)
exprToCnfSet (Literal name) = Set.singleton $ Set.singleton $ Atom name
exprToCnfSet (Negation (Literal name)) = Set.singleton $ Set.singleton $ Negated name
exprToCnfSet (Negation (Negation expr)) = exprToCnfSet expr
exprToCnfSet (Negation (And exprs)) = exprToCnfSet $ Or $ fmap Negation exprs
exprToCnfSet (Negation (Or exprs)) = exprToCnfSet $ And $ fmap Negation exprs
exprToCnfSet (Negation T) = exprToCnfSet F
exprToCnfSet (Negation F) = exprToCnfSet T
exprToCnfSet (Negation e) = exprToCnfSet $ Negation $ exprToCnf e
exprToCnfSet (And []) = Set.empty                 -- []
exprToCnfSet (Or []) = Set.singleton $ Set.empty  -- [[]]
exprToCnfSet (And [expr]) = exprToCnfSet expr
exprToCnfSet (Or [expr]) = exprToCnfSet expr
exprToCnfSet (And exprs) = Set.unions $ map exprToCnfSet exprs
exprToCnfSet (Or exprs) = distributeOr (map exprToCnfSet exprs)
exprToCnfSet (If e1 e2) = exprToCnfSet $ Or [Negation e1, e2]
exprToCnfSet (Iff e1 e2) = exprToCnfSet $ And [If e1 e2, If e2 e1]
exprToCnfSet T = Set.singleton $ Set.empty -- [[]]
exprToCnfSet F = Set.empty                 -- []
