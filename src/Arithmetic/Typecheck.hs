{-# LANGUAGE LambdaCase #-}
module Arithmetic.Typecheck where

import           Arithmetic.Syntax

typeError :: (Term, Ty) -> Ty -> Term -> String
typeError (t, ty) expected context
  = unlines
      [ "You done fucked up. Couldn't match given: "
      , show t <> " : " <> show ty
      , "with expected:"
      , show expected
      , "in: "
      , show context
      ]

typeOf :: Term -> Either String Ty
typeOf t
  = case t of
      TmTrue  -> pure TyBool
      TmFalse -> pure TyBool
      tmIf@(TmIf t1 t2 t3) -> typeOf t1 >>= \case
        TyBool -> do
          ty2 <- typeOf t2
          ty3 <- typeOf t3
          if ty2 == ty3
          then pure ty2
          else Left $ typeError (t2, ty2) ty3 tmIf
        x -> Left $ typeError (t1, x) TyBool tmIf

      TmZero -> pure TyNum

      tmSucc@(TmSucc t) -> typeOf t >>= \case
        TyNum -> pure TyNum
        x     -> Left $ typeError (t, x) TyNum tmSucc

      tmPred@(TmPred t) -> typeOf t >>= \case
        TyNum -> pure TyNum
        x     -> Left $ typeError (t, x) TyNum tmPred

      tmIsZero@(TmIsZero t) -> typeOf t >>= \case
        TyNum -> pure TyBool
        x     -> Left $ typeError (t, x) TyBool tmIsZero




