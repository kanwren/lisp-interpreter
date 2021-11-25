{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Control.Monad.Error.Class (throwError)
import TextShow (TextShow(..))

import Types

numArgs :: Symbol -> Int -> [Expr] -> Eval a
numArgs name n xs
  | got > n = throwError $ Error $ fromSymbol name <> ": too many args: expected " <> showt n <> " but got " <> showt got
  | got < n = throwError $ Error $ fromSymbol name <> ": too few args: expected " <> showt n <> " but got " <> showt got
  | otherwise = error "numArgs: impossible"
  where
    got = length xs

numArgsAtLeast :: Symbol -> Int -> [Expr] -> Eval a
numArgsAtLeast name n xs
  | got < n = throwError $ Error $ fromSymbol name <> ": too few args: expected at least " <> showt n <> " but got " <> showt got
  | otherwise = error "numArgs: impossible"
  where
    got = length xs
