module Generation
  ( generateASM,
  )
where

import Control.Monad
import Control.Monad.State
import Control.Monad.Writer.Lazy
import Data.Int (Int32)
import qualified Data.Map.Strict as Map
import Data.Monoid (Endo (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word16, Word32, Word64, Word8)
import Parser

data GeneratorState = GeneratorState
  { gsLabelCounter :: Int,
    gsVarMap :: Map.Map Text Int,
    gsStackIndex :: Int
  }
  deriving (Show, Eq)

type Generator a = StateT GeneratorState (Writer (Endo [Instruction])) a

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
        JNE label -> ["jne", show label]
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
  | RSI
  | RDI
  | RSP
  | RBP
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  | EffectiveAddr64 Displacement32 Arg64 Arg64 Scale

instance Arg Arg64

instance Show Arg64 where
  show (QWORD x) = '$' : show x
  show RAX = "%rax"
  show RBX = "%rbx"
  show RCX = "%rcx"
  show RSI = "%rsi"
  show RDI = "%rdi"
  show RSP = "%rsp"
  show RBP = "%rbp"
  show R8 = "%r8"
  show R9 = "%r9"
  show R10 = "%r10"
  show R11 = "%r11"
  show R12 = "%r12"
  show R13 = "%r13"
  show R14 = "%r14"
  show R15 = "%r15"
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
  | ESP
  | EBP
  | R8D
  | R9D
  | R10D
  | R11D
  | R12D
  | R13D
  | R14D
  | R15D
  | EffectiveAddr32 Displacement32 Arg32 Arg32 Scale

instance Arg Arg32

instance Show Arg32 where
  show (DWORD x) = '$' : show x
  show EAX = "%eax"
  show EBX = "%ebx"
  show ECX = "%ecx"
  show EDX = "%edx"
  show EDI = "%edi"
  show EBP = "%ebp"
  show ESI = "%esi"
  show ESP = "%esp"
  show R8D = "%r8d"
  show R9D = "%r9d"
  show R10D = "%r10d"
  show R11D = "%r11d"
  show R12D = "%r12d"
  show R13D = "%r13d"
  show R14D = "%r14d"
  show R15D = "%r15d"
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
generateASM prog = concatMap show
  $ flip ($) []
    . appEndo
    . execWriter
    . flip evalStateT initialGenState
  $ do
    generateFunction (getFunDecl prog)
  where
    initialGenState =
      GeneratorState
        { gsLabelCounter = 0,
          gsVarMap = Map.empty,
          gsStackIndex = 0
        }

generateFunction :: FunDecl -> Generator ()
generateFunction (Fun name statements) = do
  modify resetGenState
  tellInstrs
    [ Globl name,
      Label name
    ]
  tellInstrs
    [ PUSH RBP,
      MOVQ RSP RBP
    ]
  mapM_ generateStatement statements
  where
    resetGenState genState = genState {gsVarMap = Map.empty, gsStackIndex = 0}

generateStatement :: Statement -> Generator ()
generateStatement (Return r) = do
  generateExp r
  tellInstrs
    [ MOVQ RBP RSP,
      POP RBP
    ]
  tellInstr RET
generateStatement (Declare name mr) = do
  maybe
    (tellInstr $ MOVQ (QWORD 0) RAX)
    generateExp
    mr
  tellInstr $ PUSH RAX
  insertVar name 8
  where
    insertVar :: Text -> Int -> Generator ()
    insertVar varName varSize = do
      oldVarMap <- gets gsVarMap
      oldStackIndex <- gets gsStackIndex
      modify $ \genState ->
        genState
          { gsVarMap = Map.insert varName (oldStackIndex - varSize) oldVarMap,
            gsStackIndex = oldStackIndex - varSize
          }
generateStatement (SExp sexp) = generateExp sexp

generateExp :: Exp -> Generator ()
generateExp (Exp l []) = generateLogicalAndExp l
generateExp (Exp l ls) = do
  generateLogicalAndExp l
  forM_
    ls
    ( \(lOp, l') -> do
        case lOp of
          OrOp -> do
            clauseLabel <- uniqueLabel "orclause"
            endLabel <- uniqueLabel "end"
            tellInstrs
              [ CMP (DWORD 0) EAX,
                JE clauseLabel,
                MOVL (DWORD 1) EAX,
                JMP endLabel,
                Label clauseLabel
              ]
            generateLogicalAndExp l'
            tellInstrs
              [ CMP (DWORD 0) EAX,
                MOVQ (QWORD 0) RAX,
                SETNE AL,
                Label endLabel
              ]
    )

generateLogicalAndExp :: LogicalAndExp -> Generator ()
generateLogicalAndExp (LogicalAndExp e []) = generateEqualityExp e
generateLogicalAndExp (LogicalAndExp e es) = do
  generateEqualityExp e
  forM_
    es
    ( \(eOp, e') -> do
        case eOp of
          AndOp -> do
            clauseLabel <- uniqueLabel "andclause"
            endLabel <- uniqueLabel "end"
            tellInstrs
              [ CMP (DWORD 0) EAX,
                JNE clauseLabel,
                JMP endLabel,
                Label clauseLabel
              ]
            generateEqualityExp e'
            tellInstrs
              [ CMP (DWORD 0) EAX,
                MOVQ (QWORD 0) RAX,
                SETNE AL,
                Label endLabel
              ]
    )

generateEqualityExp :: EqualityExp -> Generator ()
generateEqualityExp (EqualityExp r []) = generateRelationalExp r
generateEqualityExp (EqualityExp r rs) = do
  generateRelationalExp r
  forM_
    rs
    ( \(rOp, r') -> do
        tellInstr $ PUSH RAX
        generateRelationalExp r'
        tellInstrs
          [ POP RCX,
            CMP EAX ECX,
            MOVQ (QWORD 0) RAX
          ]
        case rOp of
          NotEqualOp -> do
            tellInstr $ SETNE AL
          EqualOp -> do
            tellInstr $ SETE AL
    )

generateRelationalExp :: RelationalExp -> Generator ()
generateRelationalExp (RelationalExp a []) = generateAdditiveExp a
generateRelationalExp (RelationalExp a as) = do
  generateAdditiveExp a
  forM_
    as
    ( \(aOp, a') -> do
        tellInstr $ PUSH RAX
        generateAdditiveExp a'
        tellInstrs
          [ POP RCX,
            CMP EAX ECX,
            MOVQ (QWORD 0) RAX
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

generateAdditiveExp :: AdditiveExp -> Generator ()
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
            tellInstrs
              [ MOVL EAX ECX,
                POP RAX,
                SUBL ECX EAX
              ]
    )

generateTerm :: Term -> Generator ()
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
            tellInstrs
              [ MOVL EAX ECX,
                POP RAX,
                CDQ,
                IDIVL ECX
              ]
    )

generateFactor :: Factor -> Generator ()
generateFactor (Const n) = tellInstr $ MOVL (DWORD n) EAX
generateFactor (UnaryFactor op f) = do
  generateFactor f
  case op of
    NegationOp -> do
      tellInstr $ NEG EAX
    BitwiseComplementOp -> do
      tellInstr $ NOT EAX
    LogicalNegationOp -> do
      tellInstrs
        [ CMP (DWORD 0) EAX,
          MOVL (DWORD 0) EAX,
          SETE AL
        ]
generateFactor (Parens expr) = generateExp expr

tellInstr :: Instruction -> Generator ()
tellInstr = tellInstrs . pure

tellInstrs :: [Instruction] -> Generator ()
tellInstrs = tell . Endo . (++)

-- | Supplies a unique label prepended with the arguement label name
uniqueLabel :: Text -> Generator Text
uniqueLabel labelName = do
  genState <- get
  let labelCounter = gsLabelCounter genState
  put (genState {gsLabelCounter = succ labelCounter})
  return $ "_" <> labelName <> T.pack (show labelCounter)
