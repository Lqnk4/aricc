module Main where

import System.FilePath
import Control.Monad
import Data.Either
import qualified Data.Text.IO as T
import Generation
import Lexer
import Parser
import System.Exit (exitFailure)
import System.IO
import Text.Megaparsec (errorBundlePretty, runParser)
import Prelude hiding (lex)

main :: IO ()
main = do
  let sourceFile = "test/missing_semicolon.c"
  source <- T.readFile sourceFile
  tokens <-
    either
      (\e -> hPutStrLn stderr (errorBundlePretty e) >> hPutStrLn stderr "[ERROR] exited with lexer failure" >> exitFailure)
      pure
      (runParser lex sourceFile source)
  ast <-
    either
      (\e -> hPutStrLn stderr (errorBundlePretty e) >> hPutStrLn stderr "[ERROR] exited with parser failure" >> exitFailure)
      pure
      (runParser parse sourceFile $ TokenStream {tokenStreamInput = source, unTokenStream = tokens})
  prettyPrintAST ast
  let asm = generateASM ast
  T.writeFile (replaceExtension sourceFile "s") asm
  return ()
