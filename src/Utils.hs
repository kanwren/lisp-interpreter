{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Data.Text qualified as Text
import TextShow (TextShow(..))

import Types

numArgs :: Symbol -> Int -> [Expr] -> Eval a
numArgs name n xs
  | got > n = evalError $ fromSymbol name <> ": too many args: expected " <> showt n <> " but got " <> showt got
  | got < n = evalError $ fromSymbol name <> ": too few args: expected " <> showt n <> " but got " <> showt got
  | otherwise = error $ "numArgs: impossible: " ++ Text.unpack (fromSymbol name)
  where
    got = length xs

numArgsAtLeast :: Symbol -> Int -> [Expr] -> Eval a
numArgsAtLeast name n xs
  | got < n = evalError $ fromSymbol name <> ": too few args: expected at least " <> showt n <> " but got " <> showt got
  | otherwise = error $ "numArgsAtLeast: impossible: " ++ Text.unpack (fromSymbol name)
  where
    got = length xs

numArgsBound :: Symbol -> (Int, Int) -> [Expr] -> Eval a
numArgsBound name (minlen, maxlen) xs
  | got < minlen = evalError $ fromSymbol name <> ": too few args: expected at least " <> showt minlen <> " but got " <> showt got
  | got > maxlen = evalError $ fromSymbol name <> ": too many args: expected at most " <> showt maxlen <> " but got " <> showt got
  | otherwise = error $ "numArgsBound: impossible: " ++ Text.unpack (fromSymbol name)
  where
    got = length xs
