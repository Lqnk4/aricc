module Generation
  ( generateASM,
  )
where

import Control.Monad.Writer.Lazy
import Data.Int
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import Parser

-- | GNU asm instructions
data Instruction where
  MOVL :: Arg32 -> Arg32 -> Instruction
  MOVQ :: Arg64 -> Arg64 -> Instruction
  NEG :: (Arg a) => a -> Instruction
  NOT :: (Arg a) => a -> Instruction
  CMP :: (Arg a, Arg b) => a -> b -> Instruction
  SETE :: Arg8 -> Instruction
  RET :: Instruction
  -- labels for compatability with Writer [Instruction]
  Globl :: Text -> Instruction
  Label :: Text -> Instruction

instance Show Instruction where
  show (Globl name) = ".globl " ++ T.unpack name ++ "\n"
  show (Label name) = T.unpack name ++ ":\n"
  show instr = '\t' : unwords (show' instr) ++ "\n"
    where
      show' instr' = case instr' of
        MOVL arg1 arg2 -> ["movl", show arg1 ++ ",", show arg2]
        MOVQ arg1 arg2 -> ["movq", show arg1 ++ ",", show arg2]
        NEG arg1 -> ["neg", show arg1]
        NOT arg1 -> ["not", show arg1]
        CMP arg1 arg2 -> ["cmp", show arg1 ++ ",", show arg2]
        SETE arg1 -> ["sete", show arg1]
        RET -> ["ret"]
        Globl _ -> undefined
        Label _ -> undefined

type Displacement32 = Int32

data Scale = S1 | S2 | S4 | S8 deriving (Eq, Ord)

instance Show Scale where
  show = \case
    S1 -> "1"
    S2 -> "2"
    S4 -> "4"
    S8 -> "8"

class (Show a) => Arg a

data Arg64
  = QWORD Word64
  | RAX
  | RDI
  | RSI
  | EffectiveAddr64 Displacement32 Arg64 Arg64 Scale

instance Arg Arg64

instance Show Arg64 where
  show (QWORD x) = '$' : show x
  show RAX = "%rax"
  show RDI = "%rdi"
  show RSI = "%rsi"
  show (EffectiveAddr64 displacement base index scale) =
    show displacement ++ "(" ++ show base ++ ", " ++ show index ++ ", " ++ show scale ++ ")"

data Arg32
  = DWORD Word32
  | EAX
  | EDI
  | ESI
  | EffectiveAddr32 Displacement32 Arg32 Arg32 Scale

instance Arg Arg32

instance Show Arg32 where
  show (DWORD x) = '$' : show x
  show EAX = "%eax"
  show EDI = "%edi"
  show ESI = "%esi"
  show (EffectiveAddr32 displacement base index scale) =
    show displacement ++ "(" ++ show base ++ ", " ++ show index ++ ", " ++ show scale ++ ")"

data Arg16
  = WORD Word16
  | AX

instance Show Arg16 where
  show (WORD x) = '$' : show x
  show AX = "%ax"

instance Arg Arg16

data Arg8
  = BYTE Word8
  | AL
  | AH

instance Show Arg8 where
  show (BYTE x) = '$' : show x
  show AL = "%al"
  show AH = "%ah"

instance Arg Arg8

generateASM :: Program -> String
generateASM prog = concatMap show . execWriter $ do
  generateFunction (getFunDecl prog)

generateFunction :: FunDecl -> Writer [Instruction] ()
generateFunction (Fun name statement) = do
  tell $
    [ Globl name,
      Label name
    ]
  generateStatement statement

generateStatement :: Statement -> Writer [Instruction] ()
generateStatement (Return r) = do
  generateExpression r
  tellInstr RET

generateExpression :: Exp -> Writer [Instruction] ()
generateExpression (Const x) = do
  tellInstr $ MOVQ (QWORD x) RAX
generateExpression (UnaryOpExp op expr) = do
  generateExpression expr
  case op of
    NegationOp -> do
      tellInstr $ NEG RAX
    BitwiseComplementOp -> do
      tellInstr $ NOT RAX
    LogicalNegationOp -> do
      tell $
        [ CMP (QWORD 0) RAX,
          MOVQ (QWORD 0) RAX,
          SETE AL
        ]

tellInstr :: Instruction -> Writer [Instruction] ()
tellInstr = tell . pure
