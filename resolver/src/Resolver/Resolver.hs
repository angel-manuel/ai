module Resolver.Resolver (
  KnowledgeBase(..)
  , emptyKB
  , tellKB
  , askKB
  , explainKB
  , exprToCnf
  , exprToCnfArr
  , exprToCnfSet
  , LogicalExpression(..)
  , LogicalCommand(..)
) where

import Control.Monad
import Control.Monad.State.Lazy
import Data.List
import qualified Data.Set as Set

import Debug.Trace

data LogicalCommand =
  Tell LogicalExpression
  | Ask LogicalExpression;

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

cnfSetToExpr :: Set.Set (Set.Set LogicalAtom) -> LogicalExpression
cnfSetToExpr expr = And $ fmap (\o_exprs -> Or $ fmap atomToExpr o_exprs) $ map Set.toList $ Set.toList expr
  where
    atomToExpr (Atom name)    = Literal name
    atomToExpr (Negated name) = Negation (Literal name)

exprToCnf :: LogicalExpression -> LogicalExpression
exprToCnf = cnfSetToExpr.exprToCnfSet

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

resolveWith :: LogicalAtom -> Set.Set LogicalAtom -> Set.Set LogicalAtom -> Set.Set LogicalAtom
resolveWith r a b = Set.union (Set.delete (negateAtom r) a) (Set.delete r b)

resolve :: Set.Set LogicalAtom -> Set.Set LogicalAtom -> Set.Set (Set.Set LogicalAtom)
resolve a b = Set.map (\r -> resolveWith r a b) b

pairs :: Set.Set t -> [(t, t)]
pairs s = [(x,y) | (x:xt) <- tails (Set.toList s), y <- xt]

isCnfSat :: Set.Set (Set.Set LogicalAtom) -> Bool
isCnfSat kb = evalState loop kb
  where
    loop = do
      clauses <- get
      let new_clauses_arr = map (\(a, b) -> resolve a b) $ pairs clauses
      if any (== Set.singleton Set.empty) new_clauses_arr then (return False) else do
        let new = Set.unions new_clauses_arr
        if Set.isSubsetOf new clauses then (return True) else do -- FIX: Work done again next line
          put $ Set.union new clauses
          loop

type KnowledgeBase = Set.Set (Set.Set LogicalAtom)

emptyKB = Set.empty

tellKB :: LogicalExpression -> KnowledgeBase -> KnowledgeBase
tellKB expr = Set.union (exprToCnfSet expr)

askKB :: LogicalExpression -> KnowledgeBase -> Bool
askKB expr kb = not $ isCnfSat (Set.union (exprToCnfSet $ Negation expr) kb)

explainKB :: KnowledgeBase -> LogicalExpression
explainKB = cnfSetToExpr
