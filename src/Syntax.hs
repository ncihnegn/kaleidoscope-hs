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
  | If Expr Expr Expr
  | For Name Expr Expr Expr Expr
  deriving (Eq, Ord, Show)
