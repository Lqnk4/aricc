module Main where

import Data.Either
import qualified Data.Text.IO as T
import Lexer
import Parser
import Generation
import Text.Megaparsec (runParser)
import Prelude hiding (lex)

main :: IO ()
main = do
  let sourceFile = "test/return_2.c"
  source <- T.readFile sourceFile
  let tokens = fromRight [EOF] $ runParser lex sourceFile source
      ast = fromRight undefined $ runParser parse sourceFile tokens
      asm = generateASM ast
  print asm
  T.writeFile "test/return_2.s" asm
