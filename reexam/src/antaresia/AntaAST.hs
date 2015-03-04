-- | An abstract syntax tree definition for the Antaresia language.
module AntaAST
       ( Name
       , Value (..)
       , Result
       , Program
       , Decls
       , Decl
       , Args3 (..)
       , Exprs
       , Expr (..)
       , ListComp (..)
       , ListFor
       , ListIter (..)
       )
       where

-- | A name is a string.
type Name = String

-- | A value is either an integer, True, False, or a list of Values.
-- Expressions are evaluated to values.
data Value = IntVal Integer
           | TrueVal | FalseVal
           | List [Value]
           deriving (Eq, Show)

type Result = [(Name, Value)]

-- | An Antaresia program is just a list of declarations.
type Program = Decls

type Decls = [Decl]

-- | A declaration binds a name to the value of an expression
type Decl = (Name, Expr)

data Args3 = A1 Expr
           | A2 Expr Expr
           | A3 Expr Expr Expr
           deriving (Eq, Show)

type Exprs = [Expr]

-- | An Antaresia expression.
data Expr = IntConst Integer
          | TrueConst
          | FalseConst
          | Name Name
          | Range Args3
          | Plus Expr Expr
          | Minus Expr Expr
          | Mult Expr Expr
          | Div Expr Expr
          | Modulus Expr Expr
          | Equal Expr Expr
          | In Expr Expr
          | NotIn Expr Expr
          | ListComp ListComp
          deriving (Eq, Show)

data ListComp = Simple Exprs
              | ListFor Expr ListFor
              deriving (Eq, Show)

type ListFor = (Name, Expr, Maybe ListIter)

data ListIter = ListForIter ListFor
              | ListIf Expr (Maybe ListIter)
              deriving (Eq, Show)
