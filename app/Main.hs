module Main where

import Control.Monad
import Data.Either
import qualified Data.Text.IO as T
import Generation
import Lexer
import Parser
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath
import System.IO
import Text.Megaparsec (errorBundlePretty, runParser)
import Prelude hiding (lex)

main :: IO ()
main = do
  inFile <- getArgs >>= parseArgs
  source <- T.readFile inFile
  tokens <-
    either
      (\e -> hPutStrLn stderr (errorBundlePretty e) >> hPutStrLn stderr "[ERROR] exited with lexer failure" >> exitFailure)
      pure
      (runParser lex inFile source)
  ast <-
    either
      (\e -> hPutStrLn stderr (errorBundlePretty e) >> hPutStrLn stderr "[ERROR] exited with parser failure" >> exitFailure)
      pure
      (runParser parse inFile $ TokenStream {tokenStreamInput = source, unTokenStream = tokens})
  let asm = generateASM ast
  writeFile (replaceExtension inFile "s") asm
  return ()

parseArgs :: [String] -> IO FilePath
parseArgs [] = hPutStrLn stderr "[ERROR] missing input file" >> exitFailure
parseArgs [inFile] = return inFile
parseArgs (arg : _) = do
  putStrLn "[WARN] currently only the first input file provided will be compiled"
  parseArgs [arg]
