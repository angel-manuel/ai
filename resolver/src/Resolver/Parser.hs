module Resolver.Parser (
  parseExpr
) where

import Control.Applicative((<*))
import Control.Monad
import Data.List
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

import Resolver.Resolver

parseExpr :: String -> Either ParseError LogicalExpression
parseExpr = parse exprParser ""

buildPrattParser :: Stream s m t => OperatorTable s u m a -> ParsecT s u m a -> ParsecT s u m a
buildPrattParser table termP = parser precs where
  precs = reverse table
  prefixP = choice prefixPs <|> termP where
    prefixPs = do
      precsR@(ops:_) <- tails precs
      Prefix opP <- ops
      return $ opP <*> parser precsR
  infixP precs lhs = choice infixPs <|> pure lhs where
    infixPs = do
      precsR@(ops:precsL) <- tails precs
      op <- ops
      p <- case op of
        Infix opP assoc -> do
          let p precs = opP <*> pure lhs <*> parser precs
          return $ case assoc of
            AssocNone  -> error "Non associative operators are not supported"
            AssocLeft  -> p precsL
            AssocRight -> p precsR
        Postfix opP ->
          return $ opP <*> pure lhs
        Prefix _ -> mzero
      return $ p >>= infixP precs
  parser precs = prefixP >>= infixP precs

exprParser :: Parser LogicalExpression
exprParser = buildPrattParser table term <?> "expression"
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
        identStart = upper
      , identLetter = alphaNum
      , reservedNames = [ "T", "F" ]
      , reservedOpNames = [ "<=>", "=>", "^", "|", "¬", "!" ]
      }
    m_parens = parens lexer
    m_reserved = reserved lexer
    m_reservedOp name = lexeme lexer $ try $ string name
    m_identifier = identifier lexer
