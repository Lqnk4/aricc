module Generation where

import Control.Monad.Writer.Lazy
import Data.Text (Text)
import qualified Data.Text as T
import Parser

generateASM :: Prog -> Text
generateASM prog = execWriter $ do
  generateFunction (getFunDecl prog)

generateFunction :: FunDecl -> Writer Text ()
generateFunction (Fun name statement) = do
  tell . T.concat $ [".globl ", name, "\n"]
  tell . T.concat $ [name, ":", "\n"]
  generateStatement statement

generateStatement :: Statement -> Writer Text ()
generateStatement (Return r) = do
  _ <- tell . T.concat $ ["movq ", generateExpression r, ", ", "%rax", "\n"]
  tell "ret\n"

generateExpression :: Exp -> Text
generateExpression (Const x) = '$' `T.cons` T.pack (show x)
