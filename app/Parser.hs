module Parser
  ( Parser.parse,
    prettyPrintAST,
    Program (..),
    FunDecl (..),
    Statement (..),
    Exp (..),
    LogicalAndExp (..),
    LogicalAndExpOp (..),
    EqualityExp (..),
    EqualityExpOp (..),
    RelationalExp (..),
    RelationalExpOp (..),
    AdditiveExp (..),
    AdditiveExpOp (..),
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
import Data.Void (Void)
import Data.Word (Word32)
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

{- Week 4
<program> ::= <function>
<function> ::= "int" <id> "(" ")" "{" <statement> "}"
<statement> ::= "return" <exp> ";"
<exp> ::= <logical-and-exp> { "||" <logical-and-exp> }
<logical-and-exp> ::= <equality-exp> { "&&" <equality-exp> }
<equality-exp> ::= <relational-exp> { ("!=" | "==") <relational-exp> }
<relational-exp> ::= <additive-exp> { ("<" | ">" | "<=" | ">=") <additive-exp> }
<additive-exp> ::= <term> { ("+" | "-") <term> }
<term> ::= <factor> { ("*" | "/") <factor> }
<factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int>
<unary_op> ::= "!" | "~" | "-"
-}

type Parser = Parsec Void TokenStream

newtype Program where
  Program :: {getFunDecl :: FunDecl} -> Program
  deriving (Show)

data FunDecl = Fun Text Statement deriving (Show)

newtype Statement = Return Exp deriving (Show)

data Exp = Exp LogicalAndExp [(LogicalAndExpOp, LogicalAndExp)]
  deriving (Show, Eq)

data LogicalAndExp = LogicalAndExp EqualityExp [(EqualityExpOp, EqualityExp)]
  deriving (Show, Eq)

data EqualityExp = EqualityExp RelationalExp [(RelationalExpOp, RelationalExp)]
  deriving (Show, Eq)

data RelationalExp = RelationalExp AdditiveExp [(AdditiveExpOp, AdditiveExp)]
  deriving (Show, Eq)

data AdditiveExp = AdditiveExp Term [(TermOp, Term)]
  deriving (Show, Eq)

data Term = Term Factor [(FactorOp, Factor)]
  deriving (Show, Eq)

data Factor
  = Parens Exp
  | UnaryFactor UnaryOp Factor
  | Const Word32
  deriving (Show, Eq)

data LogicalAndExpOp
  = OrOp
  deriving (Show, Eq)

data UnaryOp
  = NegationOp
  | BitwiseComplementOp
  | LogicalNegationOp
  deriving (Show, Eq)

data EqualityExpOp
  = AndOp
  deriving (Show, Eq)

data RelationalExpOp
  = NotEqualOp
  | EqualOp
  deriving (Show, Eq)

data AdditiveExpOp
  = LessThanOp
  | GreaterThanOp
  | LessThanEqOp
  | GreaterThanEqOp
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

expP :: Parser Exp
expP = Exp <$> logicalAndExpP <*> go []
  where
    go ls =
      try
        ( do
            result <- (,) <$> logicalAndExpOpP <*> logicalAndExpP
            go (result : ls)
        )
        <|> pure (reverse ls)

logicalAndExpP :: Parser LogicalAndExp
logicalAndExpP = LogicalAndExp <$> equalityExpP <*> go []
  where
    go es =
      try
        ( do
            result <- (,) <$> equalityExpOpP <*> equalityExpP
            go (result : es)
        )
        <|> pure (reverse es)

equalityExpP :: Parser EqualityExp
equalityExpP = EqualityExp <$> relationalExpP <*> go []
  where
    go rs =
      try
        ( do
            result <- (,) <$> relationalExpOpP <*> relationalExpP
            go (result : rs)
        )
        <|> pure (reverse rs)

relationalExpP :: Parser RelationalExp
relationalExpP = RelationalExp <$> additiveExpP <*> go []
  where
    go as =
      try
        ( do
            result <- (,) <$> additiveExpOpP <*> additiveExpP
            go (result : as)
        )
        <|> pure (reverse as)

-- TODO: avoid call to reverse in additiveExp and termP termination call
additiveExpP :: Parser AdditiveExp
additiveExpP = AdditiveExp <$> termP <*> go []
  where
    go es =
      try
        ( do
            result <- (,) <$> termOpP <*> termP
            go (result : es)
        )
        <|> pure (reverse es)

termP :: Parser Term
termP = Term <$> factorP <*> go []
  where
    go fs =
      try
        ( do
            result <- (,) <$> factorOpP <*> factorP
            go (result : fs)
        )
        <|> pure (reverse fs)

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

logicalAndExpOpP :: Parser LogicalAndExpOp
logicalAndExpOpP = token test Set.empty
  where
    test (WithPos _ _ _ Or) = Just OrOp
    test _ = Nothing

equalityExpOpP :: Parser EqualityExpOp
equalityExpOpP = token test Set.empty
  where
    test (WithPos _ _ _ And) = Just AndOp
    test _ = Nothing

relationalExpOpP :: Parser RelationalExpOp
relationalExpOpP = token test Set.empty
  where
    test (WithPos _ _ _ NotEqual) = Just NotEqualOp
    test (WithPos _ _ _ Equal) = Just EqualOp
    test _ = Nothing

additiveExpOpP :: Parser AdditiveExpOp
additiveExpOpP = token test Set.empty
  where
    test (WithPos _ _ _ LessThan) = Just LessThanOp
    test (WithPos _ _ _ GreaterThan) = Just GreaterThanOp
    test (WithPos _ _ _ LessThanEq) = Just LessThanEqOp
    test (WithPos _ _ _ GreaterThanEq) = Just GreaterThanEqOp
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
