module Parser
  ( Parser.parse,
    prettyPrintAST,
    Program (..),
    FunDecl (..),
    Statement (..),
    Exp (..),
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

{- Week 2
<program> ::= <function>
<function> ::= "int" <id> "(" ")" "{" <statement> "}"
<statement> ::= "return" <exp> ";"
<exp> ::= <unary_op> <exp> | <int>
<unary_op> ::= "!" | "~" | "-"
-}

type Parser = Parsec Void TokenStream

newtype Program where
  Program :: {getFunDecl :: FunDecl} -> Program
  deriving (Show)

data FunDecl = Fun Text Statement deriving (Show)

newtype Statement = Return Exp deriving (Show)

data Exp
  = Const Word64
  | UnaryOpExp UnaryOp Exp
  deriving (Show)

data UnaryOp
  = NegationOp
  | BitwiseComplementOp
  | LogicalNegationOp
  deriving (Show)

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
      indentPrint n $ "RETURN" <+> showExp expr

    showExp :: Exp -> Text
    showExp (Const n) = "INT" <+> T.pack (show n)
    showExp (UnaryOpExp unaryOp newExp) = showUnaryOp unaryOp <+> "(" <> showExp newExp <> ")"

    showUnaryOp :: UnaryOp -> Text
    showUnaryOp NegationOp = "-"
    showUnaryOp BitwiseComplementOp = "~"
    showUnaryOp LogicalNegationOp = "!"

parse :: Parser Program
parse = Program <$> (beginFileP *> funDeclP <* eofP)

funDeclP :: Parser FunDecl
funDeclP =
  Fun
    <$> (intKeywordP *> identifierP)
    <*> (openParenP *> closeParenP *> between openBraceP closeBraceP statementP)

statementP :: Parser Statement
statementP = Return <$> (returnKeywordP *> expP <* semicolonP)

expP :: Parser Exp
expP =
  (Const <$> intP)
    <|> (UnaryOpExp <$> unaryOpP <*> expP)

--
-- Token Primitives
--

intP :: Parser Word64
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
    test (WithPos _ _ _ Negation) = Just NegationOp
    test (WithPos _ _ _ BitwiseComplement) = Just BitwiseComplementOp
    test (WithPos _ _ _ LogicalNegation) = Just LogicalNegationOp
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
