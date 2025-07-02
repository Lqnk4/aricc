module Lexer where

import Control.Monad
import qualified Data.Char as Char
import Data.Functor
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Token
    = OpenBrace
    | CloseBrace
    | OpenParen
    | CloseParen
    | Semicolon
    | IntKeyword
    | CharKeyword
    | ReturnKeyword
    | Identifier Text
    | IntLiteral Int
    | EOF
    deriving (Eq, Show)

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

lexEOF :: Parser Token
lexEOF = eof $> EOF

lex :: Parser [Token]
lex = sc *> go (pure [])
  where
    go :: Parser [Token] -> Parser [Token]
    go tokensList = singleton <$> lexEOF <|> ((:) <$> lexeme lexToken <*> go tokensList)

lexToken :: Parser Token
lexToken =
    choice
        [ lexOpenBrace
        , lexCloseBrace
        , lexOpenParen
        , lexCloseParen
        , lexSemicolon
        , lexIntKeyword
        , lexCharKeyword
        , lexReturnKeyword
        , lexIdentifier
        , lexIntLiteral
        ]

--
-- Tokens
--

lexOpenBrace :: Parser Token
lexOpenBrace = single '{' $> OpenBrace

lexCloseBrace :: Parser Token
lexCloseBrace = single '}' $> CloseBrace

lexOpenParen :: Parser Token
lexOpenParen = single '(' $> OpenParen

lexCloseParen :: Parser Token
lexCloseParen = single ')' $> CloseParen

lexSemicolon :: Parser Token
lexSemicolon = single ';' $> Semicolon

lexIntKeyword :: Parser Token
lexIntKeyword = chunk "int" $> IntKeyword

lexCharKeyword :: Parser Token
lexCharKeyword = chunk "char" $> CharKeyword

lexReturnKeyword :: Parser Token
lexReturnKeyword = chunk "return" $> ReturnKeyword

-- TODO: https://en.cppreference.com/w/c/language/identifiers.html
lexIdentifier :: Parser Token
lexIdentifier =
    Identifier . T.pack
        <$> ((:) <$> firstChar <*> many (alphaNumChar <|> underscore <|> unicodeEscape))
            <?> "Identifier"
  where
    underscore = char '_'
    unicodeEscape =
        toEnum . hexToInt
            <$> ((chunk "\\u" *> replicateM 4 hexDigitChar) <|> (chunk "\\U" *> replicateM 8 hexDigitChar))
    hexToInt = foldl ((+) . (16 *)) 0 . map Char.digitToInt
    firstChar = letterChar <|> underscore <|> unicodeEscape

-- TODO: Bounds checks?
-- TODO: char as int literal
lexIntLiteral :: Parser Token
lexIntLiteral = IntLiteral <$> choice [hex, oct, bin, dec] <?> "Int Literal"
  where
    hex = try $ char '0' *> char' 'x' *> L.hexadecimal
    oct = try $ char '0' *> char' 'o' *> L.octal
    bin = try $ char '0' *> char' 'b' *> L.binary
    dec = try L.decimal
