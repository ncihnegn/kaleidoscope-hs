module Syntax where

type Name = String

data Expr
  = Float Double
  | Var String
  | UnaryOp Name Expr
  | BinaryOp Name Expr Expr
  | Function Name [Name] Expr
  | Call Name [Expr]
  | Extern Name [Name]
  deriving (Eq, Ord, Show)
