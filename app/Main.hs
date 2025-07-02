module Main where

import qualified Data.Text.IO as T
import Lexer
import Prelude hiding (lex)
import Text.Megaparsec (runParser)

main :: IO ()
main = do
    let sourceFile = "test/return_2.c"
    source <- T.readFile sourceFile
    print $ runParser lex sourceFile source
