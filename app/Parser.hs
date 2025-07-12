module Parser
  ( Parser.parse,
    prettyPrintAST,
    Program (..),
    FunDecl (..),
    Statement (..),
    Exp (..),
    Term (..),
    TermOp (..),
    Factor (..),
    FactorOp (..),
    UnaryOp (..),
  )
where

import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void
import Data.Word
import Lexer
import Text.Megaparsec

{- Week 3
<program> ::= <function>
<function> ::= "int" <id> "(" ")" "{" <statement> "}"
<statement> ::= "return" <exp> ";"
<exp> ::= <term> { ("+" | "-") <term> }
<term> ::= <factor> { ("*" | "/") <factor> }
<factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int>
-}

type Parser = Parsec Void TokenStream

newtype Program where
  Program :: {getFunDecl :: FunDecl} -> Program
  deriving (Show)

data FunDecl = Fun Text Statement deriving (Show)

newtype Statement = Return Exp deriving (Show)

data Exp = Exp Term [(TermOp, Term)] deriving (Show, Eq)

data Term = Term Factor [(FactorOp, Factor)] deriving (Show, Eq)

data Factor
  = Parens Exp
  | UnaryFactor UnaryOp Factor
  | Const Word32
  deriving (Show, Eq)

data UnaryOp
  = NegationOp
  | BitwiseComplementOp
  | LogicalNegationOp
  deriving (Show, Eq)

data TermOp
  = AdditionOp
  | SubtractionOp
  deriving (Show, Eq)

data FactorOp
  = MultiplicationOp
  | DivisionOp
  deriving (Show, Eq)

-- TODO: use show instances instead
prettyPrintAST :: Program -> IO ()
prettyPrintAST prog = do
  printProg 0 prog
  where
    indentWidth = 4
    indentPrint n s = T.putStrLn $ T.replicate (indentWidth * n) " " <> s
    (<+>) s1 s2 = s1 <> " " <> s2

    printProg n program = printFunDecl n (getFunDecl program)

    printFunDecl n (Fun name statement) = do
      indentPrint n $ "FunDecl" <+> "INT" <+> name
      indentPrint (n + 1) $ "params:" <+> "()"
      indentPrint (n + 1) "body:"
      printStatement (n + 2) statement

    printStatement n (Return expr) = do
      indentPrint n $ "RETURN" <+> T.pack (show expr)

parse :: Parser Program
parse = Program <$> (beginFileP *> funDeclP <* eofP)

funDeclP :: Parser FunDecl
funDeclP =
  Fun
    <$> (intKeywordP *> identifierP)
    <*> (openParenP *> closeParenP *> between openBraceP closeBraceP statementP)

statementP :: Parser Statement
statementP = Return <$> (returnKeywordP *> expP <* semicolonP)

-- TODO: test expP and termP
expP :: Parser Exp
expP = Exp <$> termP <*> go []
  where
    go es =
      try (do
          result <- (,) <$> termOpP <*> termP
          go (result : es))
        <|> pure es

termP :: Parser Term
termP = Term <$> factorP <*> go []
  where
    go fs =
      try ( do
          result <- (,) <$> factorOpP <*> factorP
          go (result : fs)
      )
        <|> pure fs

factorP :: Parser Factor
factorP =
  Parens
    <$> between openParenP closeParenP expP
      <|> UnaryFactor
    <$> unaryOpP
    <*> factorP
      <|> Const
    <$> intP

--
-- Token Primitives
--

intP :: Parser Word32
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

unaryOpP :: Parser UnaryOp
unaryOpP = token test Set.empty
  where
    test (WithPos _ _ _ Minus) = Just NegationOp
    test (WithPos _ _ _ BitwiseComplement) = Just BitwiseComplementOp
    test (WithPos _ _ _ LogicalNegation) = Just LogicalNegationOp
    test _ = Nothing

termOpP :: Parser TermOp
termOpP = token test Set.empty
  where
    test (WithPos _ _ _ Addition) = Just AdditionOp
    test (WithPos _ _ _ Minus) = Just SubtractionOp
    test _ = Nothing

factorOpP :: Parser FactorOp
factorOpP = token test Set.empty
  where
    test (WithPos _ _ _ Multiplication) = Just MultiplicationOp
    test (WithPos _ _ _ Division) = Just DivisionOp
    test _ = Nothing

beginFileP :: Parser ()
beginFileP = token test Set.empty
  where
    test (WithPos _ _ _ BeginFile) = Just ()
    test _ = Nothing

eofP :: Parser ()
eofP = token test Set.empty
  where
    test (WithPos _ _ _ EOF) = Just ()
    test _ = Nothing
