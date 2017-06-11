module Resolver.Parser (
  parseExpr
) where

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Text.ParserCombinators.Parsec hiding (spaces)

import Resolver.Resolver

parseExpr :: String -> Either ParseError LogicalExpression
parseExpr = parse exprParser ""

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
