module Parser
  ( Parser.parse,
    Prog (..),
    FunDecl (..),
    Statement (..),
    Exp (..),
  )
where

import qualified Data.Set as Set
import Data.Text (Text)
import Data.Void
import Lexer
import Text.Megaparsec

{- Week 1
<program> ::= <function>
<function> ::= "int" <id> "(" ")" "{" <statement> "}"
<statement> ::= "return" <exp> ";"
<exp> ::= <int>
-}

type Parser = Parsec Void TokenStream

newtype Prog where
  Prog :: {getFunDecl :: FunDecl} -> Prog
  deriving (Show)

data FunDecl = Fun Text Statement deriving (Show)

newtype Statement = Return Exp deriving (Show)

newtype Exp = Const Int deriving (Show)

parse :: Parser Prog
parse = Prog <$> funDeclP <* eofP

funDeclP :: Parser FunDecl
funDeclP =
  Fun
    <$> (intKeywordP *> identifierP)
    <*> (openParenP *> closeParenP *> between openBraceP closeBraceP statementP)

statementP :: Parser Statement
statementP = Return <$> (returnKeywordP *> expP <* semicolonP)

expP :: Parser Exp
expP = Const <$> intP

--
-- Token Primitives
--

intP :: Parser Int
intP = token test Set.empty
  where
    test = \case
      (WithPos {tokenVal = IntLiteral n}) -> Just n
      _ -> Nothing

identifierP :: Parser Text
identifierP = token test Set.empty
  where
    test = \case
      (WithPos {tokenVal = Identifier name}) -> Just name
      _ -> Nothing

semicolonP :: Parser ()
semicolonP = token test Set.empty
  where
    test = \case
      (WithPos {tokenVal = Semicolon}) -> Just ()
      _ -> Nothing

openBraceP :: Parser ()
openBraceP = token test Set.empty
  where
    test = \case
      (WithPos {tokenVal = OpenBrace}) -> Just ()
      _ -> Nothing

closeBraceP :: Parser ()
closeBraceP = token test Set.empty
  where
    test = \case
      (WithPos {tokenVal = CloseBrace}) -> Just ()
      _ -> Nothing

openParenP :: Parser ()
openParenP = token test Set.empty
  where
    test = \case
      (WithPos {tokenVal = OpenParen}) -> Just ()
      _ -> Nothing

closeParenP :: Parser ()
closeParenP = token test Set.empty
  where
    test = \case
      (WithPos {tokenVal = CloseParen}) -> Just ()
      _ -> Nothing

intKeywordP :: Parser ()
intKeywordP = token test Set.empty
  where
    test = \case
      (WithPos {tokenVal = IntKeyword}) -> Just ()
      _ -> Nothing

returnKeywordP :: Parser ()
returnKeywordP = token test Set.empty
  where
    test = \case
      (WithPos {tokenVal = ReturnKeyword}) -> Just ()
      _ -> Nothing

eofP :: Parser ()
eofP = token test Set.empty
  where
    test (WithPos _ _ _ EOF) = Just ()
    test _ = Nothing
