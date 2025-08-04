module Generation
  ( generateASM,
  )
where

import Control.Monad
import Control.Monad.Writer.Lazy
import Data.Int
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word
import Parser

-- | GNU asm instructions (AT&T syntax)
data Instruction where
  MOVL :: Arg32 -> Arg32 -> Instruction
  MOVQ :: Arg64 -> Arg64 -> Instruction
  PUSH :: Arg64 -> Instruction
  POP :: Arg64 -> Instruction
  ADDL :: Arg32 -> Arg32 -> Instruction
  ADDQ :: Arg64 -> Arg64 -> Instruction
  SUBL :: Arg32 -> Arg32 -> Instruction
  SUBQ :: Arg64 -> Arg64 -> Instruction
  IMUL :: (Arg a) => a -> a -> Instruction
  IDIVL :: Arg32 -> Instruction
  IDIVQ :: Arg64 -> Instruction
  CWD :: Instruction
  CDQ :: Instruction
  CQO :: Instruction
  NEG :: (Arg a) => a -> Instruction
  NOT :: (Arg a) => a -> Instruction
  CMP :: (Arg a1, Arg a2) => a2 -> a1 -> Instruction
  -- TODO: Give labels their own type and update type signatures accordingly
  -- Also add a conversion labelToInstr :: Label -> Instruction
  JMP :: Text -> Instruction
  JE :: Text -> Instruction
  JNE :: Text -> Instruction
  SETE :: Arg8 -> Instruction
  SETNE :: Arg8 -> Instruction
  SETL :: Arg8 -> Instruction
  SETLE :: Arg8 -> Instruction
  SETG :: Arg8 -> Instruction
  SETGE :: Arg8 -> Instruction
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
        PUSH arg1 -> ["push", show arg1]
        POP arg1 -> ["pop", show arg1]
        ADDL arg1 arg2 -> ["addl", show arg1 ++ ",", show arg2]
        ADDQ arg1 arg2 -> ["addq", show arg1 ++ ",", show arg2]
        SUBL arg1 arg2 -> ["subl", show arg1 ++ ",", show arg2]
        SUBQ arg1 arg2 -> ["subq", show arg1 ++ ",", show arg2]
        IMUL arg1 arg2 -> ["imul", show arg1 ++ ",", show arg2]
        IDIVL arg1 -> ["idivl", show arg1]
        IDIVQ arg1 -> ["idivq", show arg1]
        CWD -> ["cwd"]
        CDQ -> ["cdq"]
        CQO -> ["cqo"]
        NEG arg1 -> ["neg", show arg1]
        NOT arg1 -> ["not", show arg1]
        CMP arg1 arg2 -> ["cmp", show arg1 ++ ",", show arg2]
        JMP label -> ["jmp", show label]
        JE label -> ["je", show label]
        JNE label -> ["je", show label]
        SETE arg1 -> ["sete", show arg1]
        SETNE arg1 -> ["setne", show arg1]
        SETL arg1 -> ["setl", show arg1]
        SETLE arg1 -> ["setle", show arg1]
        SETG arg1 -> ["setg", show arg1]
        SETGE arg1 -> ["setge", show arg1]
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
  | RBX
  | RCX
  | RDI
  | RSI
  | EffectiveAddr64 Displacement32 Arg64 Arg64 Scale

instance Arg Arg64

instance Show Arg64 where
  show (QWORD x) = '$' : show x
  show RAX = "%rax"
  show RBX = "%rbx"
  show RCX = "%rcx"
  show RDI = "%rdi"
  show RSI = "%rsi"
  show (EffectiveAddr64 displacement base index scale) =
    show displacement ++ "(" ++ show base ++ ", " ++ show index ++ ", " ++ show scale ++ ")"

data Arg32
  = DWORD Word32
  | EAX
  | EBX
  | ECX
  | EDX
  | ESI
  | EDI
  | EBP
  | EffectiveAddr32 Displacement32 Arg32 Arg32 Scale

instance Arg Arg32

instance Show Arg32 where
  show (DWORD x) = '$' : show x
  show EAX = "%eax"
  show EBX = "%ebx"
  show ECX = "%ecx"
  show EDX = "%edx"
  show EDI = "%edi"
  show ESI = "%esi"
  show EBP = "%ebp"
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
  tell
    [ Globl name,
      Label name
    ]
  generateStatement statement

generateStatement :: Statement -> Writer [Instruction] ()
generateStatement (Return r) = do
  generateExp r
  tellInstr RET

generateExp :: Exp -> Writer [Instruction] ()
generateExp (Exp l []) = generateLogicalAndExp l
generateExp (Exp l ls) = undefined

generateLogicalAndExp :: LogicalAndExp -> Writer [Instruction] ()
generateLogicalAndExp (LogicalAndExp e []) = generateEqualityExp e
generateLocalAndExp _ = undefined
{-generateLogicalAndExp (LogicalAndExp e es) = do
  generateEqualityExp e
  forM_
    es
    ( \(eOp, e') -> do
      case eOp of
        AndOp -> do
          -- TODO: function to generate unique arbitrary labels
          let clauseLabel = generateLabelText "andclause"
          let endLabel = generateLabelText "endclause"
          tell
            [ CMP (DWORD 0) EAX
            , JNE clauseLabel
            , JMP endLabel
            , Label clauseLabel
            ]
          generateEqualityExp e'
          tell
            [ CMP (DWORD 0) EAX
            , MOVQ (QWORD 0) RAX
            , SETNE AL
            ]
    )
-}

generateEqualityExp :: EqualityExp -> Writer [Instruction] ()
generateEqualityExp (EqualityExp r []) = generateRelationalExp r
generateEqualityExp (EqualityExp r rs) = do
  generateRelationalExp r
  forM_
    rs
    ( \(rOp, r') -> do
      tellInstr $ PUSH RAX
      generateRelationalExp r'
      tell
        [ POP RCX
        , CMP EAX ECX
        , MOVQ (QWORD 0) RAX
        ]
      case rOp of
        NotEqualOp -> do
          tellInstr $ SETE AL
        EqualOp -> do
          tellInstr $ SETNE AL
    )

generateRelationalExp :: RelationalExp -> Writer [Instruction] ()
generateRelationalExp (RelationalExp a []) = generateAdditiveExp a
generateRelationalExp (RelationalExp a as) = do
  generateAdditiveExp a
  forM_
    as
    ( \(aOp, a') -> do
        tellInstr $ PUSH RAX
        generateAdditiveExp a'
        tell
          [ POP RCX
          , CMP EAX ECX
          , MOVQ (QWORD 0) RAX
          ]
        case aOp of
          LessThanOp -> do
            tellInstr $ SETL AL
          LessThanEqOp -> do
            tellInstr $ SETLE AL
          GreaterThanOp -> do
            tellInstr $ SETG AL
          GreaterThanEqOp -> do
            tellInstr $ SETGE AL
    )

generateAdditiveExp :: AdditiveExp -> Writer [Instruction] ()
generateAdditiveExp (AdditiveExp t []) = generateTerm t
generateAdditiveExp (AdditiveExp t ts) = do
  generateTerm t
  forM_
    ts
    ( \(tOp, t') -> do
        tellInstr $ PUSH RAX
        generateTerm t'
        case tOp of
          AdditionOp -> do
            tellInstr $ POP RCX
            tellInstr $ ADDL ECX EAX
          SubtractionOp -> do
            tell
              [ MOVL EAX ECX,
                POP RAX,
                SUBL ECX EAX
              ]
    )

generateTerm :: Term -> Writer [Instruction] ()
generateTerm (Term f []) = generateFactor f
generateTerm (Term f fs) = do
  generateFactor f
  forM_
    fs
    ( \(fOp, f') -> do
        tellInstr $ PUSH RAX
        generateFactor f'
        case fOp of
          MultiplicationOp -> do
            tellInstr $ POP RCX
            tellInstr $ IMUL ECX EAX
          DivisionOp -> do
            tell
              [ MOVL EAX ECX,
                POP RAX,
                CDQ,
                IDIVL ECX
              ]
    )

generateFactor :: Factor -> Writer [Instruction] ()
generateFactor (Const n) = tellInstr $ MOVL (DWORD n) EAX
generateFactor (UnaryFactor op f) = do
  generateFactor f
  case op of
    NegationOp -> do
      tellInstr $ NEG EAX
    BitwiseComplementOp -> do
      tellInstr $ NOT EAX
    LogicalNegationOp -> do
      tell
        [ CMP (DWORD 0) EAX,
          MOVL (DWORD 0) EAX,
          SETE AL
        ]
generateFactor (Parens expr) = generateExp expr

tellInstr :: Instruction -> Writer [Instruction] ()
tellInstr = tell . pure
