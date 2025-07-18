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
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Data.Word
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
  | Identifier Text
  | IntLiteral Word32
  | Minus
  | BitwiseComplement
  | LogicalNegation
  | Addition
  | Multiplication
  | Division
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
  (Identifier name) -> name
  (IntLiteral n) -> T.pack (show n)
  Minus -> "-"
  BitwiseComplement -> "~"
  LogicalNegation -> "!"
  Addition -> "+"
  Multiplication -> "*"
  Division -> "/"
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
          lexBitwiseComplement,
          lexLogicalNegation,
          lexAddition,
          lexMultiplication,
          lexDivision
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

-- TODO: char as int literal
lexIntLiteral :: Lexer CToken
lexIntLiteral = lexeme (IntLiteral <$> choice [hex, oct, bin, dec] <?> "Int Literal")
  where
    hex = try $ char '0' *> char' 'x' *> L.hexadecimal
    oct = try $ char '0' *> char' 'o' *> L.octal
    bin = try $ char '0' *> char' 'b' *> L.binary
    dec = try L.decimal

lexMinus :: Lexer CToken
lexMinus = symbol "-" $> Minus

lexBitwiseComplement :: Lexer CToken
lexBitwiseComplement = symbol "~" $> BitwiseComplement

lexLogicalNegation :: Lexer CToken
lexLogicalNegation = symbol "!" $> LogicalNegation

lexAddition :: Lexer CToken
lexAddition = symbol "+" $> Addition

lexMultiplication :: Lexer CToken
lexMultiplication = symbol "*" $> Multiplication

lexDivision :: Lexer CToken
lexDivision = symbol "/" $> Division
