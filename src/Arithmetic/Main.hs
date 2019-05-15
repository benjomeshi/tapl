module Arithmetic.Main where

import           Arithmetic.Eval
import           Arithmetic.Syntax
import           Arithmetic.Typecheck

main :: IO ()
main = do
  testTerm <- readLn

  case typeOf testTerm of
    Left err   -> putStr err
    Right nice -> putStrLn $ show testTerm <> " : " <> show nice

  case eval1 testTerm of
    Nothing   -> putStrLn $ "eval of " <> show testTerm <> " failed"
    Just nice -> putStrLn $ show testTerm <> " -> " <> show nice
