{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Parser where

import Data.Set as Set
import Data.Text (Text)
import Data.Void
import Lexer
import Text.Megaparsec hiding (Token)

{- Week 1
<program> ::= <function>
<function> ::= "int" <id> "(" ")" "{" <statement> "}"
<statement> ::= "return" <exp> ";"
<exp> ::= <int>
-}

type Parser = Parsec Void [Token]

newtype Prog where
    Prog :: { getFunDecl :: FunDecl } -> Prog deriving (Show)

data FunDecl = Fun Text Statement deriving (Show)

newtype Statement = Return Exp deriving (Show)

newtype Exp = Const Int deriving (Show)

parse :: Parser Prog
parse = Prog <$> funDeclP

funDeclP :: Parser FunDecl
funDeclP =
    Fun
        <$> (single IntKeyword *> token testIdentifier Set.empty)
        <*> (single OpenParen *> single CloseParen *> between (single OpenBrace) (single CloseBrace) statementP)
  where
    testIdentifier = \case
        Identifier name -> Just name
        _ -> Nothing

statementP :: Parser Statement
statementP = Return <$> (single ReturnKeyword *> expP <* single Semicolon)

expP :: Parser Exp
expP = Const <$> token testIntLiteral Set.empty
  where
    testIntLiteral = \case
        IntLiteral x -> Just x
        _ -> Nothing
