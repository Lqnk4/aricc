module Parser where

data Program = Program FunctionDecl

data FunctionDecl = Function String Statement

data Statement
    = Return Expression
    | Assign Variable Expression

data Expression = Constant Int

data Variable = Variable String


