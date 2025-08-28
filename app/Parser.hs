module Parser
  ( Parser.parse,
    prettyPrintAST,
    Program (..),
    FunDecl (..),
    Statement (..),
    CExp (..),
    Exp (..),
    LogicalAndExp (..),
    LogicalAndExpOp (..),
    BitwiseOrExp (..),
    BitwiseOrExpOp (..),
    BitwiseXorExp (..),
    BitwiseXorExpOp (..),
    BitwiseAndExp (..),
    BitwiseAndExpOp (..),
    EqualityExp (..),
    EqualityExpOp (..),
    RelationalExp (..),
    RelationalExpOp (..),
    BitwiseExp (..),
    BitwiseExpOp (..),
    AdditiveExp (..),
    AdditiveExpOp (..),
    Term (..),
    TermOp (..),
    Factor (..),
    FactorOp (..),
    UnaryOp (..),
  )
where

import Control.Monad
import Control.Monad.State
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import Data.Word (Word32)
import Lexer
import Text.Megaparsec hiding (State)

newtype ParserState
  = ParserState {psLocalVars :: Set.Set Text}

type Parser = StateT ParserState (Parsec Void TokenStream)

newtype Program where
  Program :: {getFunDecl :: FunDecl} -> Program
  deriving (Show)

data FunDecl = Fun Text [Statement] deriving (Show)

data Statement
  = Return Exp
  | Declare Text (Maybe Exp)
  | SExp Exp
  deriving (Show)

-- TODO: Use literally any other implementation
-- that allows combining expressions and creating literals easily
-- this is god awful
class CExp e e' e'op | e -> e', e -> e'op where
  liftCExp :: e' -> e
  toExp :: e -> Exp
  liftConst :: Word32 -> e
  {-# MINIMAL liftCExp, toExp, liftConst #-}

data Exp
  = Assign Text Exp
  | AdditionAssign Text Exp
  | SubtractionAssign Text Exp
  | MultiplicationAssign Text Exp
  | DivisionAssign Text Exp
  | ModulusAssign Text Exp
  | BitwiseLShiftAssign Text Exp
  | BitwiseRShiftAssign Text Exp
  | BitwiseANDAssign Text Exp
  | BitwiseORAssign Text Exp
  | BitwiseXORAssign Text Exp
  | Exp LogicalAndExp [(LogicalAndExpOp, LogicalAndExp)]
  deriving (Show, Eq)

instance CExp Exp LogicalAndExp LogicalAndExpOp where
  liftCExp lae = Exp lae []
  liftConst = liftCExp . liftConst
  toExp = id

data LogicalAndExpOp
  = OrOp
  deriving (Show, Eq)

data LogicalAndExp = LogicalAndExp BitwiseOrExp [(BitwiseOrExpOp, BitwiseOrExp)]
  deriving (Show, Eq)

instance CExp LogicalAndExp BitwiseOrExp BitwiseOrExpOp where
  liftCExp boe = LogicalAndExp boe []
  liftConst = liftCExp . liftConst
  toExp = liftCExp

data BitwiseOrExpOp
  = AndOp
  deriving (Show, Eq)

data BitwiseOrExp = BitwiseOrExp BitwiseXorExp [(BitwiseXorExpOp, BitwiseXorExp)]
  deriving (Show, Eq)

instance CExp BitwiseOrExp BitwiseXorExp BitwiseXorExpOp where
  liftCExp bxe = BitwiseOrExp bxe []
  liftConst = liftCExp . liftConst
  toExp = liftCExp . liftCExp

data BitwiseXorExpOp
  = BitwiseOrOp
  deriving (Show, Eq)

data BitwiseXorExp = BitwiseXorExp BitwiseAndExp [(BitwiseAndExpOp, BitwiseAndExp)]
  deriving (Show, Eq)

instance CExp BitwiseXorExp BitwiseAndExp BitwiseAndExpOp where
  liftCExp bae = BitwiseXorExp bae []
  liftConst = liftCExp . liftConst
  toExp = liftCExp . liftCExp . liftCExp

data BitwiseAndExpOp
  = BitwiseXorOp
  deriving (Show, Eq)

data BitwiseAndExp = BitwiseAndExp EqualityExp [(EqualityExpOp, EqualityExp)]
  deriving (Show, Eq)

instance CExp BitwiseAndExp EqualityExp EqualityExpOp where
  liftCExp bae = BitwiseAndExp bae []
  liftConst = liftCExp . liftConst
  toExp = liftCExp . liftCExp . liftCExp . liftCExp

data EqualityExpOp
  = BitwiseAndOp
  deriving (Show, Eq)

data EqualityExp = EqualityExp RelationalExp [(RelationalExpOp, RelationalExp)]
  deriving (Show, Eq)

instance CExp EqualityExp RelationalExp RelationalExpOp where
  liftCExp bae = EqualityExp bae []
  liftConst = liftCExp . liftConst
  toExp = liftCExp . liftCExp . liftCExp . liftCExp . liftCExp

data RelationalExpOp
  = NotEqualOp
  | EqualOp
  deriving (Show, Eq)

data RelationalExp = RelationalExp BitwiseExp [(BitwiseExpOp, BitwiseExp)]
  deriving (Show, Eq)

instance CExp RelationalExp BitwiseExp BitwiseExpOp where
  liftCExp bae = RelationalExp bae []
  liftConst = liftCExp . liftConst
  toExp = liftCExp . liftCExp . liftCExp . liftCExp . liftCExp . liftCExp

data BitwiseExpOp
  = LessThanOp
  | LessThanEqOp
  | GreaterThanOp
  | GreaterThanEqOp
  deriving (Show, Eq)

data BitwiseExp = BitwiseExp AdditiveExp [(AdditiveExpOp, AdditiveExp)]
  deriving (Show, Eq)

instance CExp BitwiseExp AdditiveExp AdditiveExpOp where
  liftCExp bae = BitwiseExp bae []
  liftConst = liftCExp . liftConst
  toExp = liftCExp . liftCExp . liftCExp . liftCExp . liftCExp . liftCExp . liftCExp

data AdditiveExpOp
  = BitwiseLShiftOp
  | BitwiseRShiftOp
  deriving (Show, Eq)

data AdditiveExp = AdditiveExp Term [(TermOp, Term)]
  deriving (Show, Eq)

instance CExp AdditiveExp Term TermOp where
  liftCExp bae = AdditiveExp bae []
  liftConst = liftCExp . liftConst
  toExp = liftCExp . liftCExp . liftCExp . liftCExp . liftCExp . liftCExp . liftCExp . liftCExp

data TermOp
  = AdditionOp
  | SubtractionOp
  deriving (Show, Eq)

data Term = Term Factor [(FactorOp, Factor)]
  deriving (Show, Eq)

instance CExp Term Factor FactorOp where
  liftCExp bae = Term bae []
  liftConst = liftCExp . Const
  toExp = liftCExp . liftCExp . liftCExp . liftCExp . liftCExp . liftCExp . liftCExp . liftCExp . liftCExp

data FactorOp
  = MultiplicationOp
  | DivisionOp
  | ModulusOp
  deriving (Show, Eq)

data Factor
  = Parens Exp
  | UnaryFactor UnaryOp Factor
  | Const Word32
  | Var Text
  deriving (Show, Eq)

instance CExp Factor Exp Void where
  liftCExp = Parens
  liftConst = Const
  toExp (Parens expr) = expr
  toExp expr =
    liftCExp . liftCExp . liftCExp . liftCExp . liftCExp . liftCExp . liftCExp . liftCExp . liftCExp . liftCExp $ expr

data UnaryOp
  = NegationOp
  | BitwiseComplementOp
  | LogicalNegationOp
  deriving (Show, Eq)

-- TODO: use StateT for indent level and just make it look nice
prettyPrintAST :: Program -> IO ()
prettyPrintAST prog = do
  printProg 0 prog
  where
    indentWidth = 4
    indentPrint n s = T.putStrLn $ T.replicate (indentWidth * n) " " <> s
    (<+>) s1 s2 = s1 <> " " <> s2

    printProg n program = printFunDecl n (getFunDecl program)

    printFunDecl n (Fun name statements) = do
      indentPrint n $ "FunDecl" <+> "INT" <+> name
      indentPrint (n + 1) $ "params:" <+> "()"
      indentPrint (n + 1) "body:"
      forM_ statements $ \statement -> do
        printStatement (n + 2) statement

    printStatement n (Return expr) = do
      indentPrint n $ "RETURN" <+> T.pack (show expr)
    printStatement n (Declare name expr) = do
      indentPrint n $ "DECLARE" <+> name <+> T.pack (show expr)
    printStatement n (SExp expr) = do
      indentPrint n (T.pack (show expr))

parse :: Parsec Void TokenStream Program
parse = evalStateT (Program <$> (beginFileP *> funDeclP <* eofP)) initialState
  where
    initialState = ParserState {psLocalVars = Set.empty}

funDeclP :: Parser FunDecl
funDeclP = do
  modify $ \ps -> ps {psLocalVars = Set.empty}
  funName <- intKeywordP *> identifierP
  offset <- getOffset
  statements <- openParenP *> closeParenP *> between openBraceP closeBraceP (many statementP)
  statements' <- case unsnoc statements of
    Just (_, Return _) -> pure statements
    _ ->
      if funName == "main"
        then pure $ statements ++ [Return exp0]
        else do
          registerParseError $
            TrivialError
              offset
              (Just . Label $ ' ' :| "function `" ++ T.unpack funName ++ "` missing return statement")
              Set.empty
          return statements
  return $ Fun funName statements'
  where
    unsnoc :: [a] -> Maybe ([a], a)
    unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
    {-# INLINEABLE unsnoc #-}
    exp0 :: Exp
    exp0 = liftConst 0

statementP :: Parser Statement
statementP =
  Return
    <$> (returnKeywordP *> expP <* semicolonP)
      <|> SExp
    <$> (expP <* semicolonP)
      -- Declare
      <|> do
        offset <- getOffset
        identifier <- intKeywordP *> identifierP
        mexp <- (Just <$> between assignmentP semicolonP expP) <|> (Nothing <$ semicolonP)
        localVars <- gets psLocalVars
        if Set.member identifier localVars
          then
            registerParseError $
              TrivialError
                offset
                (Just . Label $ ' ' :| "duplicate variable declaration of `" ++ T.unpack identifier ++ "`")
                Set.empty
          else modify $ \ps -> ps {psLocalVars = Set.insert identifier localVars}
        return $ Declare identifier mexp

expP :: Parser Exp
expP =
  parseAssign assignmentP Assign
    <|> parseAssign additionAssignmentP AdditionAssign
    <|> parseAssign subtractionAssignmentP SubtractionAssign
    <|> parseAssign multiplicationAssignmentP MultiplicationAssign
    <|> parseAssign divisionAssignmentP DivisionAssign
    <|> parseAssign modulusAssignmentP ModulusAssign
    <|> parseAssign bitwiseLShiftAssignmentP BitwiseLShiftAssign
    <|> parseAssign bitwiseRShiftAssignmentP BitwiseRShiftAssign
    <|> parseAssign bitwiseANDAssignmentP BitwiseANDAssign
    <|> parseAssign bitwiseORAssignmentP BitwiseORAssign
    <|> parseAssign bitwiseXORAssignmentP BitwiseXORAssign
    <|> (Exp <$> logicalAndExpP <*> go [])
  where
    parseAssign :: Parser a -> (Text -> Exp -> Exp) -> Parser Exp
    parseAssign assignmentOpP assignConstructor =
      try
        ( do
            offset <- getOffset
            identifier <- identifierP <* assignmentOpP
            expr <- expP
            localVars <- gets psLocalVars
            unless (Set.member identifier localVars) $ do
              registerParseError $
                TrivialError
                  offset
                  (Just . Label $ ' ' :| "assignment to undeclared variable `" ++ T.unpack identifier ++ "`")
                  Set.empty
            return $ assignConstructor identifier expr
        )
    go ls =
      try
        ( do
            result <- (,) <$> logicalAndExpOpP <*> logicalAndExpP
            go (result : ls)
        )
        <|> pure (reverse ls)

logicalAndExpP :: Parser LogicalAndExp
logicalAndExpP = LogicalAndExp <$> bitwiseOrExpP <*> go []
  where
    go es =
      try
        ( do
            result <- (,) <$> bitwiseOrExpOpP <*> bitwiseOrExpP
            go (result : es)
        )
        <|> pure (reverse es)

bitwiseOrExpP :: Parser BitwiseOrExp
bitwiseOrExpP = BitwiseOrExp <$> bitwiseXorExpP <*> go []
  where
    go es =
      try
        ( do
            result <- (,) <$> bitwiseXorExpOpP <*> bitwiseXorExpP
            go (result : es)
        )
        <|> pure (reverse es)

bitwiseXorExpP :: Parser BitwiseXorExp
bitwiseXorExpP = BitwiseXorExp <$> bitwiseAndExpP <*> go []
  where
    go es =
      try
        ( do
            result <- (,) <$> bitwiseAndExpOpP <*> bitwiseAndExpP
            go (result : es)
        )
        <|> pure (reverse es)

bitwiseAndExpP :: Parser BitwiseAndExp
bitwiseAndExpP = BitwiseAndExp <$> equalityExpP <*> go []
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
relationalExpP = RelationalExp <$> bitwiseExpP <*> go []
  where
    go as =
      try
        ( do
            result <- (,) <$> bitwiseExpOpP <*> bitwiseExpP
            go (result : as)
        )
        <|> pure (reverse as)

bitwiseExpP :: Parser BitwiseExp
bitwiseExpP = BitwiseExp <$> additiveExpP <*> go []
  where
    go as =
      try
        ( do
            result <- (,) <$> additiveExpOpP <*> additiveExpP
            go (result : as)
        )
        <|> pure (reverse as)

-- TODO: avoid call to reverse in expression parser termination call
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
      <|> Var
    <$> identifierP

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

assignmentP :: Parser ()
assignmentP = token test Set.empty
  where
    test = \case
      (WithPos {tokenVal = Assignment}) -> Just ()
      _ -> Nothing

additionAssignmentP :: Parser ()
additionAssignmentP = token test Set.empty
  where
    test = \case
      (WithPos {tokenVal = AdditionAssignment}) -> Just ()
      _ -> Nothing

subtractionAssignmentP :: Parser ()
subtractionAssignmentP = token test Set.empty
  where
    test = \case
      (WithPos _ _ _ SubtractionAssignment) -> Just ()
      _ -> Nothing

multiplicationAssignmentP :: Parser ()
multiplicationAssignmentP = token test Set.empty
  where
    test = \case
      (WithPos _ _ _ MultiplicationAssignment) -> Just ()
      _ -> Nothing

divisionAssignmentP :: Parser ()
divisionAssignmentP = token test Set.empty
  where
    test = \case
      (WithPos _ _ _ DivisionAssignment) -> Just ()
      _ -> Nothing

modulusAssignmentP :: Parser ()
modulusAssignmentP = token test Set.empty
  where
    test = \case
      (WithPos _ _ _ ModulusAssignment) -> Just ()
      _ -> Nothing

bitwiseLShiftAssignmentP :: Parser ()
bitwiseLShiftAssignmentP = token test Set.empty
  where
    test = \case
      (WithPos _ _ _ BitwiseLShiftAssignment) -> Just ()
      _ -> Nothing

bitwiseRShiftAssignmentP :: Parser ()
bitwiseRShiftAssignmentP = token test Set.empty
  where
    test = \case
      (WithPos _ _ _ BitwiseRShiftAssignment) -> Just ()
      _ -> Nothing

bitwiseANDAssignmentP :: Parser ()
bitwiseANDAssignmentP = token test Set.empty
  where
    test = \case
      (WithPos _ _ _ BitwiseANDAssignment) -> Just ()
      _ -> Nothing

bitwiseORAssignmentP :: Parser ()
bitwiseORAssignmentP = token test Set.empty
  where
    test = \case
      (WithPos _ _ _ BitwiseORAssignment) -> Just ()
      _ -> Nothing

bitwiseXORAssignmentP :: Parser ()
bitwiseXORAssignmentP = token test Set.empty
  where
    test = \case
      (WithPos _ _ _ BitwiseXORAssignment) -> Just ()
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

bitwiseOrExpOpP :: Parser BitwiseOrExpOp
bitwiseOrExpOpP = token test Set.empty
  where
    test (WithPos _ _ _ And) = Just AndOp
    test _ = Nothing

bitwiseXorExpOpP :: Parser BitwiseXorExpOp
bitwiseXorExpOpP = token test Set.empty
  where
    test (WithPos _ _ _ BitwiseOr) = Just BitwiseOrOp
    test _ = Nothing

bitwiseAndExpOpP :: Parser BitwiseAndExpOp
bitwiseAndExpOpP = token test Set.empty
  where
    test (WithPos _ _ _ BitwiseXOR) = Just BitwiseXorOp
    test _ = Nothing

equalityExpOpP :: Parser EqualityExpOp
equalityExpOpP = token test Set.empty
  where
    test (WithPos _ _ _ BitwiseAnd) = Just BitwiseAndOp
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
    test (WithPos _ _ _ BitwiseLeftShift) = Just BitwiseLShiftOp
    test (WithPos _ _ _ BitwiseRightShift) = Just BitwiseRShiftOp
    test _ = Nothing

bitwiseExpOpP :: Parser BitwiseExpOp
bitwiseExpOpP = token test Set.empty
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
    test (WithPos _ _ _ Modulo) = Just ModulusOp
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
