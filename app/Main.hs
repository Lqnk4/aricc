module Main where

import qualified Data.Text.IO as T
import Lexer
import Parser
import Prelude hiding (lex)
import Text.Megaparsec (runParser)

main :: IO ()
main = do
    let sourceFile = "test/return_2.c"
    source <- T.readFile sourceFile
    let tokens = runParser lex sourceFile source
        ast = runParser parse sourceFile <$> tokens
    print ast
