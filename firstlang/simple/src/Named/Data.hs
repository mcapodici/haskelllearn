module Named.Data where

type Name = String

data Expression =
    Func  Name       Expression
  | Call  Expression Expression
  | Var   Name
  deriving Show

