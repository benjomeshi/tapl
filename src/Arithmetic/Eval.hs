module Arithmetic.Eval where

import           Arithmetic.Syntax

isNumericVal :: Term -> Bool
isNumericVal t =
  case t of
    TmZero   -> True
    TmSucc t -> isNumericVal t
    _        -> False

isBoolVal :: Term -> Bool
isBoolVal t =
  case t of
    TmTrue  -> True
    TmFalse -> True
    _       -> False

isVal :: Term -> Bool
isVal t =
  isBoolVal t || isNumericVal t

eval1 :: Term -> Maybe Term
eval1 t =
  case t of
    TmIf TmTrue t1 _ ->
      Just t1
    TmIf TmFalse _ t2 ->
      Just t2

    TmIf t1 t2 t3 -> do
      t1' <- eval1 t1
      pure $ TmIf t1' t2 t3

    TmSucc t1 -> do
      t1' <- eval1 t1
      pure $ TmSucc t1'

    TmPred TmZero ->
      Just TmZero

    TmPred (TmSucc t1) | isNumericVal t1 ->
      Just t1

    TmPred t1 ->
      let t1' = eval1 t1
      in TmPred <$> t1'

    TmIsZero TmZero ->
      Just TmTrue

    TmIsZero (TmSucc t1) | isNumericVal t1 ->
      Just TmFalse

    TmIsZero t1 ->
      let t1' = eval1 t1
      in TmIsZero <$> t1'

    _ ->
      Nothing

eval :: Term -> Term
eval t =
  let t' = eval1 t
  in case t' of
    Just t'' ->
      eval t''
    Nothing ->
      t
