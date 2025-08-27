module Lexer
  ( CToken (..),
    WithPos (..),
    LexerToken,
    TokenStream (..),
    Lexer.lex,
    liftCTokenP,
  )
where

import Control.Monad
import qualified Data.Char as Char
import Data.Functor
import qualified Data.List as DL
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Data.Word (Word32)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Lexer = Parsec Void Text

data CToken
  = OpenBrace
  | CloseBrace
  | OpenParen
  | CloseParen
  | Semicolon
  | IntKeyword
  | CharKeyword
  | ReturnKeyword
  | Assignment
  | Identifier Text
  | IntLiteral Word32
  | Minus
  | BitwiseComplement
  | BitwiseXOR
  | LogicalNegation
  | Addition
  | Multiplication
  | And
  | BitwiseAnd
  | Or
  | BitwiseOr
  | Equal
  | NotEqual
  | BitwiseLeftShift
  | BitwiseRightShift
  | LessThan
  | LessThanEq
  | GreaterThan
  | GreaterThanEq
  | Division
  | Modulo
  | BeginFile
  | EOF
  deriving (Eq, Ord, Show)

showCToken :: CToken -> Text
showCToken = \case
  OpenBrace -> "{"
  CloseBrace -> "}"
  OpenParen -> "("
  CloseParen -> ")"
  Semicolon -> ";"
  IntKeyword -> "int"
  CharKeyword -> "char"
  ReturnKeyword -> "return"
  Assignment -> "="
  (Identifier name) -> name
  (IntLiteral n) -> T.pack (show n)
  Minus -> "-"
  BitwiseComplement -> "~"
  BitwiseXOR -> "^"
  LogicalNegation -> "!"
  Addition -> "+"
  Multiplication -> "*"
  Division -> "/"
  Modulo -> "%"
  And -> "&&"
  BitwiseAnd -> "&"
  Or -> "||"
  BitwiseOr -> "|"
  Equal -> "=="
  NotEqual -> "!="
  BitwiseLeftShift -> "<<"
  BitwiseRightShift -> ">>"
  LessThan -> "<"
  LessThanEq -> "<="
  GreaterThan -> ">"
  GreaterThanEq -> ">="
  BeginFile -> "<Start>"
  EOF -> "<EOF>"

data WithPos a = WithPos
  { startPos :: SourcePos,
    endPos :: SourcePos,
    tokenLength :: Int,
    tokenVal :: a
  }
  deriving (Eq, Ord, Show)

type LexerToken = WithPos CToken

data TokenStream = TokenStream
  { tokenStreamInput :: Text,
    unTokenStream :: [LexerToken]
  }
  deriving (Show)

pxy :: Proxy TokenStream
pxy = Proxy

instance Stream TokenStream where
  type Token TokenStream = LexerToken
  type Tokens TokenStream = [LexerToken]
  tokenToChunk Proxy x = [x]
  tokensToChunk Proxy xs = xs
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  take1_ (TokenStream _ []) = Nothing
  take1_ (TokenStream txt (t : ts)) =
    Just
      ( t,
        TokenStream (T.drop (tokensLength pxy (t :| [])) txt) ts
      )
  takeN_ n (TokenStream txt s)
    | n <= 0 = Just ([], TokenStream txt s)
    | null s = Nothing
    | otherwise =
        let (x, s') = splitAt n s
         in case NE.nonEmpty x of
              Nothing -> Just (x, TokenStream txt s')
              Just nex -> Just (x, TokenStream (T.drop (tokensLength pxy nex) txt) s')
  takeWhile_ f (TokenStream txt s) =
    let (x, s') = DL.span f s
     in case NE.nonEmpty x of
          Nothing -> (x, TokenStream txt s')
          Just nex -> (x, TokenStream (T.drop (tokensLength pxy nex) txt) s')

instance VisualStream TokenStream where
  showTokens Proxy = T.unpack . T.unwords . NE.toList . fmap (showCToken . tokenVal)
  tokensLength Proxy xs = sum (tokenLength <$> xs)

instance TraversableStream TokenStream where
  reachOffset o PosState {..} =
    ( Just (prefix ++ T.unpack restOfLine),
      PosState
        { pstateInput =
            TokenStream
              { tokenStreamInput = postStr,
                unTokenStream = post
              },
          pstateOffset = max pstateOffset o,
          pstateSourcePos = newSourcePos,
          pstateTabWidth = pstateTabWidth,
          pstateLinePrefix = prefix
        }
    )
    where
      prefix =
        if sameLine
          then pstateLinePrefix ++ T.unpack preLine
          else T.unpack preLine
      sameLine = sourceLine newSourcePos == sourceLine pstateSourcePos
      newSourcePos =
        case post of
          [] -> case unTokenStream pstateInput of
            [] -> pstateSourcePos
            xs -> endPos (last xs)
          (x : _) -> startPos x
      (pre, post) = splitAt (o - pstateOffset) (unTokenStream pstateInput)
      -- NOTE: don't know how this works
      (preStr, postStr) = T.splitAt (tokensConsumed + 1) (tokenStreamInput pstateInput)
      preLine = T.reverse . T.takeWhile (/= '\n') . T.reverse $ preStr
      tokensConsumed =
        case NE.nonEmpty pre of
          Nothing -> 0
          Just nePre -> tokensLength pxy nePre
      restOfLine = T.takeWhile (/= '\n') postStr

liftCTokenP :: Lexer CToken -> Lexer LexerToken
liftCTokenP lexer = do
  startPos <- getSourcePos
  (parsed, tokenVal) <- match lexer
  endPos <- getSourcePos
  let tokenLength = T.length parsed
  return WithPos {..}

sc :: Lexer ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Lexer a -> Lexer a
lexeme = L.lexeme sc

symbol :: Text -> Lexer Text
symbol = L.symbol sc

keyword :: Text -> Lexer Text
keyword p = lexeme (try $ string p <* delimiter)
  where
    -- allowed chars after a keyword
    delimiter = lookAhead (choice [void $ char ';', void spaceChar, eof])

-- required for correct error offset handling in TokenStream
lexBeginFile :: Lexer LexerToken
lexBeginFile = liftCTokenP $ sc $> BeginFile

lexEOF :: Lexer LexerToken
lexEOF = liftCTokenP $ eof $> EOF

lex :: Lexer [LexerToken]
lex = do
  beginFile <- lexBeginFile
  result <- go (pure [] :: Lexer [LexerToken])
  return $ beginFile : result
  where
    go tokensList = DL.singleton <$> lexEOF <|> ((:) <$> liftCTokenP lexToken <*> go tokensList)
    lexToken =
      choice
        [ lexOpenBrace,
          lexCloseBrace,
          lexOpenParen,
          lexCloseParen,
          lexSemicolon,
          lexIntKeyword,
          lexCharKeyword,
          lexReturnKeyword,
          lexIdentifier,
          lexIntLiteral,
          lexMinus,
          lexAddition,
          lexMultiplication,
          lexDivision,
          lexModulo,
          lexAnd,
          lexBitwiseAnd,
          lexOr,
          lexBitwiseOr,
          lexEqual,
          lexNotEqual,
          lexBitwiseLeftShift,
          lexBitwiseRightShift,
          lexLessThanEq,
          lexLessThan,
          lexGreaterThanEq,
          lexGreaterThan,
          lexBitwiseComplement,
          lexBitwiseXOR,
          lexLogicalNegation,
          lexAssignment
        ]

--
-- Lexer Primitives
--

lexOpenBrace :: Lexer CToken
lexOpenBrace = symbol "{" $> OpenBrace

lexCloseBrace :: Lexer CToken
lexCloseBrace = symbol "}" $> CloseBrace

lexOpenParen :: Lexer CToken
lexOpenParen = symbol "(" $> OpenParen

lexCloseParen :: Lexer CToken
lexCloseParen = symbol ")" $> CloseParen

lexSemicolon :: Lexer CToken
lexSemicolon = symbol ";" $> Semicolon

lexIntKeyword :: Lexer CToken
lexIntKeyword = keyword "int" $> IntKeyword

lexCharKeyword :: Lexer CToken
lexCharKeyword = keyword "char" $> CharKeyword

lexReturnKeyword :: Lexer CToken
lexReturnKeyword = keyword "return" $> ReturnKeyword

lexAssignment :: Lexer CToken
lexAssignment = symbol "=" $> Assignment

-- TODO: https://en.cppreference.com/w/c/language/identifiers.html
lexIdentifier :: Lexer CToken
lexIdentifier =
  lexeme
    ( Identifier . T.pack
        <$> ((:) <$> firstChar <*> many (alphaNumChar <|> underscore <|> unicodeEscape))
        <?> "Identifier"
    )
  where
    underscore = char '_'
    unicodeEscape =
      toEnum . hexToInt
        <$> ( (chunk "\\u" *> replicateM 4 hexDigitChar)
                <|> (chunk "\\U" *> replicateM 8 hexDigitChar)
            )
    hexToInt = foldl ((+) . (16 *)) 0 . map Char.digitToInt
    firstChar = letterChar <|> underscore <|> unicodeEscape

lexIntLiteral :: Lexer CToken
lexIntLiteral = lexeme (IntLiteral <$> choice [hex, oct, bin, dec] <?> "Int Literal")
  where
    hex = try $ char '0' *> char' 'x' *> L.hexadecimal
    oct = try $ char '0' *> char' 'o' *> L.octal
    -- NOTE: binary literal not part of C standard, usually implemented as compiler extension
    bin = try $ char '0' *> char' 'b' *> L.binary
    dec = try L.decimal

lexMinus :: Lexer CToken
lexMinus = symbol "-" $> Minus

lexBitwiseComplement :: Lexer CToken
lexBitwiseComplement = symbol "~" $> BitwiseComplement

lexBitwiseXOR :: Lexer CToken
lexBitwiseXOR = symbol "^" $> BitwiseXOR

lexLogicalNegation :: Lexer CToken
lexLogicalNegation = symbol "!" $> LogicalNegation

lexAddition :: Lexer CToken
lexAddition = symbol "+" $> Addition

lexMultiplication :: Lexer CToken
lexMultiplication = symbol "*" $> Multiplication

lexDivision :: Lexer CToken
lexDivision = symbol "/" $> Division

lexModulo :: Lexer CToken
lexModulo = symbol "%" $> Modulo

lexAnd :: Lexer CToken
lexAnd = symbol "&&" $> And

lexBitwiseAnd :: Lexer CToken
lexBitwiseAnd = symbol "&" $> BitwiseAnd

lexOr :: Lexer CToken
lexOr = symbol "||" $> Or

lexBitwiseOr :: Lexer CToken
lexBitwiseOr = symbol "|" $> BitwiseOr

lexEqual :: Lexer CToken
lexEqual = symbol "==" $> Equal

lexNotEqual :: Lexer CToken
lexNotEqual = symbol "!=" $> NotEqual

lexBitwiseLeftShift :: Lexer CToken
lexBitwiseLeftShift = symbol "<<" $> BitwiseLeftShift

lexBitwiseRightShift :: Lexer CToken
lexBitwiseRightShift = symbol ">>" $> BitwiseRightShift

lexLessThan :: Lexer CToken
lexLessThan = symbol "<" $> LessThan

lexLessThanEq :: Lexer CToken
lexLessThanEq = symbol "<=" $> LessThanEq

lexGreaterThan :: Lexer CToken
lexGreaterThan = symbol ">" $> GreaterThan

lexGreaterThanEq :: Lexer CToken
lexGreaterThanEq = symbol ">=" $> GreaterThanEq
