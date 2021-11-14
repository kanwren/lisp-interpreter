{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Builtins (mkBuiltins) where

import Control.Monad (foldM, zipWithM)
import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class
import Data.Functor (($>))
import Data.IORef (newIORef)
import Data.Map.Strict qualified as Map
import TextShow (TextShow(..))

import Types
import Utils
import qualified Data.Text as Text

type Builtin = [Expr] -> Eval Expr

mkBuiltins :: IO Context
mkBuiltins = Context . Map.fromList <$> traverse ctxCell
  [ ("+", iadd)
  , ("-", isub)
  , ("*", imul)
  , ("/", idiv)
  , ("mod", imod)
  , ("quot", iquot)
  , ("rem", irem)
  , ("=", ieq)
  , ("/=", ine)
  , (">", igt)
  , ("<", ilt)
  , (">=", ige)
  , ("<=", ile)
  , ("equal", equal)
  , ("cons", cons)
  , ("car", car)
  , ("cdr", cdr)
  , ("null", primNull)
  , ("length", primLength)
  , ("char", primChar)
  , ("print", printExpr)
  ]
  where
    ctxCell (name, bi) = do
      ref <- newIORef (LBuiltin bi)
      pure (name, ref)

    intFold :: Symbol -> (b -> Integer -> b) -> b -> [Expr] -> Eval b
    intFold name f = foldM go
      where
        go total (LInt n) = pure $ f total n
        go _ e = throwError $ Error $ fromSymbol name <> ": not a number: " <> showt e

    intFold1 :: Symbol -> (Integer -> Integer -> Integer) -> [Expr] -> Eval Expr
    intFold1 name _ [] = throwError $ Error $ fromSymbol name <> ": expected at least 1 argument, got 0"
    intFold1 name f (LInt x:xs) = LInt <$> intFold name f x xs
    intFold1 name _ (x:_) = throwError $ Error $ fromSymbol name <> ": not a number: " <> showt x

    intFoldPairwise :: Symbol -> (b -> (Integer, Integer) -> b) -> b -> [Expr] -> Eval b
    intFoldPairwise name _ _ [] = throwError $ Error $ fromSymbol name <> ": expected at least 1 argument, got 0"
    intFoldPairwise name f z (LInt x:xs) = go z x xs
      where
        go total _ [] = pure total
        go total prev (LInt y:ys) = go (f total (prev, y)) y ys
        go _ _ (e:_) = throwError $ Error $ fromSymbol name <> ": not a number: " <> showt e
    intFoldPairwise name _ _ (x:_) = throwError $ Error $ fromSymbol name <> ": not a number: " <> showt x

    comparison :: Symbol -> (Integer -> Integer -> Bool) -> Builtin
    comparison name f = fmap LBool . intFoldPairwise name (\b (x, y) -> b && f x y) True

    iadd, isub, imul, idiv, imod, iquot, irem :: Builtin
    iadd = fmap LInt . intFold "+" (+) 0
    isub = intFold1 "-" (-)
    imul = fmap LInt . intFold "*" (*) 1
    idiv = intFold1 "/" div
    imod = intFold1 "mod" mod
    iquot = intFold1 "quot" quot
    irem = intFold1 "rem" rem
    ieq = comparison "=" (==)
    ine = comparison "/=" (/=)
    igt = comparison ">" (>)
    ilt = comparison "<" (<)
    ige = comparison ">=" (>=)
    ile = comparison "<=" (<=)

    cons :: Builtin
    cons [x, LList y] = pure $ LList (x:y)
    cons [x, LDottedList y z] = pure $ LDottedList (x:y) z
    cons [x, y] = pure $ LDottedList [x] y
    cons args = numArgs "cons" 2 args

    car :: Builtin
    car [LList []] = throwError $ Error "car: empty list"
    car [LList (x:_)] = pure x
    car [_] = throwError $ Error "car: expected list"
    car args = numArgs "car" 1 args

    cdr :: Builtin
    cdr [LList []] = throwError $ Error "cdr: empty list"
    cdr [LList (_:xs)] = pure $ LList xs
    cdr [_] = throwError $ Error "cdr: expected list"
    cdr args = numArgs "cdr" 1 args

    primNull :: Builtin
    primNull [LList []] = pure $ LBool True
    primNull [_] = pure $ LBool False
    primNull args = numArgs "null" 1 args

    primLength :: Builtin
    primLength [LList xs] = pure $ LInt $ fromIntegral $ length xs
    primLength [LString xs] = pure $ LInt $ fromIntegral $ Text.length xs
    primLength [_] = throwError $ Error "length: expected a sequence"
    primLength args = numArgs "length" 1 args

    primChar :: Builtin
    primChar [LString t, LInt n] =
      if n < 0 || n >= fromIntegral (Text.length t)
      then throwError $ Error $ "char: index " <> showt n <> " out of bounds"
      else pure $ LChar $ Text.index t (fromIntegral n)
    primChar [LString _, _] = throwError $ Error "char: expected string as first argument"
    primChar [_, _] = throwError $ Error "char: expected int as second argument"
    primChar args = numArgs "char" 2 args

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
        equal' x y = throwError $ Error $ "equal: incompatible types " <> renderType x <> " and " <> renderType y

    printExpr :: Builtin
    printExpr [e] = liftIO (print e) $> e
    printExpr args = numArgs "print" 1 args


