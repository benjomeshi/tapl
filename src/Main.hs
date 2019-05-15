module Main where

data Term =
  TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term
  deriving (Eq, Show)

main :: IO ()
main = print $ eval (TmPred (TmSucc TmZero))


isNumericVal :: Term -> Bool
isNumericVal t =
  case t of
    TmZero -> True
    TmSucc t -> isNumericVal t
    _ -> False

isBoolVal :: Term -> Bool
isBoolVal t =
  case t of
    TmTrue -> True
    TmFalse -> True
    _ -> False

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
    TmIf t1 t2 t3 ->
      let t1' = eval1 t1
      in TmIf <$> t1' <*> Just t2 <*> Just t3
    TmSucc t1 ->
      let t1' = eval1 t1
      in TmSucc <$> t1'
    TmPred TmZero ->
      Just TmZero
    TmPred (TmSucc t1) ->
      if isNumericVal t1 then Just t1 else Nothing
    TmPred t1 ->
      let t1' = eval1 t1
      in TmPred <$> t1'
    TmIsZero TmZero ->
      Just TmTrue
    TmIsZero (TmSucc t1) ->
      if isNumericVal t1 then Just TmFalse else Nothing
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


-- ($) :: (a -> b) -> a -> b
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b
--
-- class Applicative f where
--   (<*>) :: f (a -> b) -> f a -> f b
--
-- instance Applicative Maybe where
--   (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
--   (Just f) <*> (Just a) =
--     Just (f a)
--   (Nothing) <*> _ =
--     Nothing
--
-- instance Functor Maybe where
--   fmap :: (a -> b) -> Maybe a -> Maybe b
--   fmap f ma =
--     case ma of
--       Just a ->
--           Just (f a)
--       Nothing ->
--         Nothing
