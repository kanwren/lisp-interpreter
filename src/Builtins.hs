{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module Builtins (mkBuiltins) where

import Control.Monad (foldM, zipWithM)
import Control.Monad.IO.Class
import Data.Functor (($>))
import Data.IORef (newIORef)
import Data.Map.Strict qualified as Map
import TextShow (TextShow(..))

import Types
import Utils
import Eval (apply, eval, setVar, nil)
import qualified Data.Text as Text

type Builtin = [Expr] -> Eval Expr

mkBuiltins :: IO Context
mkBuiltins = Context . Map.fromList <$> traverse ctxCell
  [ ("+", LBuiltin iadd)
  , ("-", LBuiltin isub)
  , ("*", LBuiltin imul)
  , ("/", LBuiltin idiv)
  , ("mod", LBuiltin imod)
  , ("quot", LBuiltin iquot)
  , ("rem", LBuiltin irem)
  , ("=", LBuiltin ieq)
  , ("/=", LBuiltin ine)
  , (">", LBuiltin igt)
  , ("<", LBuiltin ilt)
  , (">=", LBuiltin ige)
  , ("<=", LBuiltin ile)
  , ("equal", LBuiltin equal)
  , ("set", LBuiltin primSet)
  , ("eval", LBuiltin primEval)
  , ("list", LBuiltin list)
  , ("apply", LBuiltin primApply)
  , ("cons", LBuiltin cons)
  , ("nil", nil)
  , ("car", LBuiltin car)
  , ("cdr", LBuiltin cdr)
  , ("null", LBuiltin primNull)
  , ("length", LBuiltin primLength)
  , ("char", LBuiltin primChar)
  , ("string=", LBuiltin stringEq)
  , ("string>", LBuiltin stringGt)
  , ("string<", LBuiltin stringLt)
  , ("string>=", LBuiltin stringGe)
  , ("string<=", LBuiltin stringLe)
  , ("print", LBuiltin printExpr)
  ]
  where
    ctxCell (name, bi) = (name,) <$> newIORef bi

    intFold :: Symbol -> (b -> Integer -> b) -> b -> [Expr] -> Eval b
    intFold name f = foldM go
      where
        go total (LInt n) = pure $ f total n
        go _ e = evalError $ fromSymbol name <> ": not a number: " <> showt e

    intFold1 :: Symbol -> (Integer -> Integer -> Integer) -> [Expr] -> Eval Expr
    intFold1 name _ [] = evalError $ fromSymbol name <> ": expected at least 1 argument, got 0"
    intFold1 name f (LInt x:xs) = LInt <$> intFold name f x xs
    intFold1 name _ (x:_) = evalError $ fromSymbol name <> ": not a number: " <> showt x

    intFoldPairwise :: Symbol -> (b -> (Integer, Integer) -> b) -> b -> [Expr] -> Eval b
    intFoldPairwise name _ _ [] = evalError $ fromSymbol name <> ": expected at least 1 argument, got 0"
    intFoldPairwise name f z (LInt x:xs) = go z x xs
      where
        go total _ [] = pure total
        go total prev (LInt y:ys) = go (f total (prev, y)) y ys
        go _ _ (e:_) = evalError $ fromSymbol name <> ": not a number: " <> showt e
    intFoldPairwise name _ _ (x:_) = evalError $ fromSymbol name <> ": not a number: " <> showt x

    comparison :: Symbol -> (Integer -> Integer -> Bool) -> Builtin
    comparison name f = fmap LBool . intFoldPairwise name (\b (x, y) -> b && f x y) True

    iadd, isub, imul, idiv, imod, iquot, irem :: Builtin
    iadd = fmap LInt . intFold "+" (+) 0
    -- unary should be negation
    isub [LInt x] = pure $ LInt (-x)
    isub args = intFold1 "-" (-) args
    imul = fmap LInt . intFold "*" (*) 1
    -- unary should be reciprocal
    -- TODO: update once rationals are added
    idiv [LInt x] = pure $ LInt (1 `div` x)
    idiv args = intFold1 "/" div args
    imod = intFold1 "mod" mod
    iquot = intFold1 "quot" quot
    irem = intFold1 "rem" rem
    ieq = comparison "=" (==)
    ine = comparison "/=" (/=)
    igt = comparison ">" (>)
    ilt = comparison "<" (<)
    ige = comparison ">=" (>=)
    ile = comparison "<=" (<=)

    primSet :: Builtin
    primSet [LSymbol name, value] = setVar name value $> value
    primSet [_, _] = evalError "set: expected symbol as variable name"
    primSet args = numArgs "set" 2 args

    primEval :: Builtin
    primEval [e] = eval e
    primEval args = numArgs "eval" 1 args

    -- could be (defun list xs xs)
    list :: Builtin
    list xs = pure $ LList xs

    primApply :: Builtin
    primApply [LFun f, LList xs] = apply f xs
    primApply [LFun _, _] = evalError "apply: expected list for arguments"
    primApply [_, _] = evalError "apply: expected function"
    primApply args = numArgs "apply" 2 args

    cons :: Builtin
    cons [x, LList y] = pure $ LList (x:y)
    cons [x, LDottedList y z] = pure $ LDottedList (x:y) z
    cons [x, y] = pure $ LDottedList [x] y
    cons args = numArgs "cons" 2 args

    car :: Builtin
    car [LList []] = evalError "car: empty list"
    car [LList (x:_)] = pure x
    car [_] = evalError "car: expected list"
    car args = numArgs "car" 1 args

    cdr :: Builtin
    cdr [LList []] = evalError "cdr: empty list"
    cdr [LList (_:xs)] = pure $ LList xs
    cdr [_] = evalError "cdr: expected list"
    cdr args = numArgs "cdr" 1 args

    primNull :: Builtin
    primNull [LList []] = pure $ LBool True
    primNull [_] = pure $ LBool False
    primNull args = numArgs "null" 1 args

    primLength :: Builtin
    primLength [LList xs] = pure $ LInt $ fromIntegral $ length xs
    primLength [LString xs] = pure $ LInt $ fromIntegral $ Text.length xs
    primLength [_] = evalError "length: expected a sequence"
    primLength args = numArgs "length" 1 args

    primChar :: Builtin
    primChar [LString t, LInt n] =
      if n < 0 || n >= fromIntegral (Text.length t)
      then evalError $ "char: index " <> showt n <> " out of bounds"
      else pure $ LChar $ Text.index t (fromIntegral n)
    primChar [LString _, _] = evalError "char: expected string as first argument"
    primChar [_, _] = evalError "char: expected int as second argument"
    primChar args = numArgs "char" 2 args

    stringComparison :: Symbol -> (forall e. Ord e => e -> e -> Bool) -> Builtin
    stringComparison _ cmp [LString x, LString y] = pure $ LBool (cmp x y)
    stringComparison _ cmp [LKeyword x, LKeyword y] = pure $ LBool (cmp x y)
    stringComparison _ cmp [LSymbol x, LSymbol y] = pure $ LBool (cmp x y)
    stringComparison _ cmp [LChar x, LChar y] = pure $ LBool (cmp x y)
    stringComparison name _ [x, y] = evalError $ fromSymbol name <> ": invalid argument types " <> renderType x <> " and " <> renderType y
    stringComparison name _ args = numArgs name 2 args

    stringEq, stringGt, stringLt, stringGe, stringLe :: Builtin
    stringEq = stringComparison "string=" (==)
    stringGt = stringComparison "string>" (>)
    stringLt = stringComparison "string<" (<)
    stringGe = stringComparison "string>=" (<=)
    stringLe = stringComparison "string<=" (<=)

    equal :: Builtin
    equal args =
      case args of
        [x, y] -> LBool <$> equal' x y
        _ -> numArgs "equal" 2 args
      where
        equal' :: Expr -> Expr -> Eval Bool
        equal' (LInt x) (LInt y) = pure (x == y)
        equal' (LBool x) (LBool y) = pure (x == y)
        equal' (LChar x) (LChar y) = pure (x == y)
        equal' (LKeyword x) (LKeyword y) = pure (x == y)
        equal' (LString x) (LString y) = pure (x == y)
        equal' (LSymbol x) (LSymbol y) = pure (x == y)
        equal' (LList x) (LList y) =
          if length x /= length y
          then pure False
          else and <$> zipWithM equal' x y
        equal' (LDottedList x x') (LDottedList y y') =
          if length x /= length y
          then pure False
          else (&&) <$> (and <$> zipWithM equal' x y) <*> equal' x' y'
        equal' (LBuiltin _) (LBuiltin _) = pure False
        equal' (LFun _) (LFun _) = pure False
        equal' x y = evalError $ "equal: incompatible types " <> renderType x <> " and " <> renderType y

    printExpr :: Builtin
    printExpr [e] = liftIO (print e) $> e
    printExpr args = numArgs "print" 1 args
