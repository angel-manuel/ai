module Main where

import Control.Arrow
import Control.Monad
import Data.List
import Text.ParserCombinators.Parsec hiding (spaces)

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

import System.Console.Haskeline

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
precendence (Iff _ _) = 4

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
compose_expr _ (Or []) = "[|]"
compose_expr prec (Negation e) = "¬" ++ repr_expr prec e
compose_expr prec (And (he:le)) = (repr_expr prec he) ++ (concat $ map (\e ->  "^" ++ (repr_expr prec e)) le)
compose_expr prec (Or (he:le)) = (repr_expr prec he) ++ (concat $ map (\e ->  "|" ++ (repr_expr prec e)) le)
compose_expr prec (If e1 e2) = (repr_expr prec e1) ++ "=>" ++ (repr_expr prec e2)
compose_expr prec (Iff e1 e2) = (repr_expr prec e1) ++ "<=>" ++ (repr_expr prec e2)

show_expr :: LogicalExpression -> String
show_expr expr = compose_expr (precendence expr) expr

exprParser :: Parser LogicalExpression
exprParser = buildExpressionParser table term <?> "expression"
  where
    table = [
        [prefix "¬" Negation, prefix "!" Negation]
      , [binary "^" (\a b -> And [a, b]), binary "|" (\a b -> Or [a, b])]
      , [binary "=>" If, binary "<=" (flip If)]
      , [binary "<=>" Iff]
      ]
    term = m_parens exprParser
      <|> (m_reserved "T" >> return T)
      <|> (m_reserved "F" >> return F)
      <|> Literal <$> m_identifier
    prefix name fun = Prefix (m_reservedOp name >> return fun)
    binary name fun = Infix  (m_reservedOp name >> return fun) AssocLeft
    lexer = makeTokenParser $ emptyDef {
        commentStart = "/*"
      , commentEnd = "/*"
      , commentLine = "//"
      , identStart = upper
      , identLetter = alphaNum
      , reservedNames = [ "T", "F" ]
      , reservedOpNames = [ "<=>", "=>", "^", "|", "¬", "!" ]
      }
    m_parens = parens lexer
    m_reserved = reserved lexer
    m_reservedOp = reservedOp lexer
    m_identifier = identifier lexer

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
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      einput <- getInputLine "resolver> "
      case einput of
        Nothing ->      return ()
        Just "quit" ->  return ()
        Just input -> do
          case parse exprParser "" input of
            (Right expr)  -> outputStrLn $ show_expr (expr_to_cnf expr)
            _             -> outputStrLn "ParseError"
          loop
