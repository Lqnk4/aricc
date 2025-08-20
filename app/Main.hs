module Main where

import Control.Monad
import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import qualified Data.Text.IO as T
import Generation
import Lexer
import Parser
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath
import System.IO
import Text.Megaparsec
  ( ParseErrorBundle (..),
    ShowErrorComponent,
    TraversableStream (..),
    VisualStream (..),
    errorBundlePretty,
    runParser,
  )
import Prelude hiding (lex)

data Env = Env
  { inFile :: FilePath,
    outFile :: Maybe FilePath
  }

type Compiler = ReaderT Env IO

main :: IO ()
main = do
  env <- getArgs >>= parseArgs
  flip runReaderT env $ do
    inFile <- asks inFile
    source <- liftIO $ T.readFile inFile
    tokens <-
      either
        (megaparsecError "exited with lexer failure")
        pure
        (runParser lex inFile source)
    ast <-
      either
        (megaparsecError "exited with parser failure")
        pure
        (runParser parse inFile $ TokenStream {tokenStreamInput = source, unTokenStream = tokens})
    let asm = generateASM ast
    outFile <- asks (fromMaybe (replaceExtension inFile "s") . outFile)
    liftIO $ writeFile outFile asm
    return ()

parseArgs :: [String] -> IO Env
parseArgs [] = hPutStrLn stderr "[ERROR] missing input file" >> exitFailure
parseArgs [inFile] = return $ Env {inFile, outFile = Nothing}
parseArgs [inFile, "-o", outFile] = return $ Env {inFile, outFile = Just outFile}
parseArgs (arg : _) = do
  putStrLn "[WARN] currently only the first input file provided will be compiled"
  parseArgs [arg]

megaparsecError ::
  (MonadIO m, VisualStream s, TraversableStream s, ShowErrorComponent e) =>
  String ->
  ParseErrorBundle s e ->
  m a
megaparsecError errMessage err =
  liftIO $
    hPutStrLn stderr (errorBundlePretty err)
      >> hPutStrLn stderr ("[ERROR] " ++ errMessage)
      >> exitFailure
