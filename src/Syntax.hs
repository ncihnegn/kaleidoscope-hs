module Syntax where

type Name = String

data Expr
  = Float Double
  | Var String
  | UnaryOp Name Expr
  | BinaryOp Name Expr Expr
  | UnaryDef Name [Name] Expr
  | BinaryDef Name [Name] Expr
  | Function Name [Name] Expr
  | Call Name [Expr]
  | Extern Name [Name]
  | If Expr Expr Expr
  | For Name Expr Expr Expr Expr
  | Let Name Expr Expr
  deriving (Eq, Ord, Show)
