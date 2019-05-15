module Arithmetic.Syntax where

data Ty
  = TyBool
  | TyNum
  deriving (Eq, Show, Read)

data Term
  = TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term
  deriving (Eq, Show, Read)

