{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Lexer (
    Token (..),
    Lexer.lex,
) where

import Control.Monad
import qualified Data.Char as Char
import Data.Functor
import Data.List
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Lexer = Parsec Void Text

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
    deriving (Eq, Ord, Show)

instance VisualStream [Token] where
    showTokens Proxy = show

sc :: Lexer ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Lexer a -> Lexer a
lexeme = L.lexeme sc

lexEOF :: Lexer Token
lexEOF = eof $> EOF

lex :: Lexer [Token]
lex = sc *> go (pure [])
  where
    go :: Lexer [Token] -> Lexer [Token]
    go tokensList = singleton <$> lexEOF <|> ((:) <$> lexeme lexToken <*> go tokensList)

lexToken :: Lexer Token
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

lexOpenBrace :: Lexer Token
lexOpenBrace = single '{' $> OpenBrace

lexCloseBrace :: Lexer Token
lexCloseBrace = single '}' $> CloseBrace

lexOpenParen :: Lexer Token
lexOpenParen = single '(' $> OpenParen

lexCloseParen :: Lexer Token
lexCloseParen = single ')' $> CloseParen

lexSemicolon :: Lexer Token
lexSemicolon = single ';' $> Semicolon

lexIntKeyword :: Lexer Token
lexIntKeyword = chunk "int" $> IntKeyword

lexCharKeyword :: Lexer Token
lexCharKeyword = chunk "char" $> CharKeyword

lexReturnKeyword :: Lexer Token
lexReturnKeyword = chunk "return" $> ReturnKeyword

-- TODO: https://en.cppreference.com/w/c/language/identifiers.html
lexIdentifier :: Lexer Token
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
lexIntLiteral :: Lexer Token
lexIntLiteral = IntLiteral <$> choice [hex, oct, bin, dec] <?> "Int Literal"
  where
    hex = try $ char '0' *> char' 'x' *> L.hexadecimal
    oct = try $ char '0' *> char' 'o' *> L.octal
    bin = try $ char '0' *> char' 'b' *> L.binary
    dec = try L.decimal
