{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Builtins (mkBuiltins) where

import Control.Monad (zipWithM)
import Control.Monad.IO.Class
import Data.Bifunctor (second)
import Data.Functor (($>), (<&>))
import Data.IORef (newIORef, IORef)
import Data.List (foldl', foldl1')
import Data.Map.Strict qualified as Map
import Data.Ratio ((%), numerator, denominator)
import Data.Text qualified as Text
import System.Exit qualified as Exit
import TextShow (TextShow(..))

import Errors
import Eval (apply, eval, setVar, nil, progn)
import Parser (parseFile)
import Types

type Builtin = [Expr] -> Eval Expr

mkBuiltins :: IO (IORef Context)
mkBuiltins = newIORef . Context . Map.fromList =<< traverse ctxCell (concat builtins)
  where
    ctxCell (name, bi) = (name,) <$> newIORef bi
    builtins = [builtinPrims, builtinPreds, builtinDefs]

builtinPrims :: [(Symbol, Expr)]
builtinPrims = fmap (second LBuiltin)
  [ ("+", iadd)
  , ("-", isub)
  , ("*", imul)
  , ("/", idiv)
  , ("div", iidiv)
  , ("mod", imod)
  , ("quot", iquot)
  , ("rem", irem)
  , ("numerator", primNumerator)
  , ("denominator", primDenominator)
  , ("=", ieq)
  , ("/=", ine)
  , (">", igt)
  , ("<", ilt)
  , (">=", ige)
  , ("<=", ile)
  , ("not", primNot)
  , ("equal", equal)
  , ("set", primSet)
  , ("eval", primEval)
  , ("list", list)
  , ("apply", primApply)
  , ("cons", cons)
  , ("car", car)
  , ("cdr", cdr)
  , ("null", primNull)
  , ("length", primLength)
  , ("char", primChar)
  , ("string=", stringEq)
  , ("string>", stringGt)
  , ("string<", stringLt)
  , ("string>=", stringGe)
  , ("string<=", stringLe)
  , ("type-of", typeOf)
  , ("print", printExpr)
  , ("load", load)
  , ("exit", exit)
  ]
  where
    asInts :: Symbol -> [Expr] -> Eval [Integer]
    asInts name = go []
      where
        go acc [] = pure $ reverse acc
        go acc (LInt x:xs) = go (x:acc) xs
        go _ (e:_) = evalError $ fromSymbol name <> ": not a number: " <> renderType e

    toRatio :: Symbol -> Expr -> Eval Rational
    toRatio _ (LInt n) = pure $ n % 1
    toRatio _ (LRatio n) = pure n
    toRatio name e = evalError $ fromSymbol name <> ": not a number: " <> renderType e

    promote :: Symbol -> [Expr] -> Eval (Either [Rational] [Integer])
    promote name = go []
      where
        go :: [Integer] -> [Expr] -> Eval (Either [Rational] [Integer])
        go acc [] = pure $ Right $ reverse acc
        go acc (LInt x:xs) = go (x:acc) xs
        go acc (LRatio x:xs) = do
          rest <- traverse (toRatio name) xs
          pure $ Left $ reverse (fmap (1 %) acc) ++ x:rest
        go _ (e:_) = evalError $ fromSymbol name <> ": not a number: " <> renderType e

    iadd :: Builtin
    iadd args = promote "+" args <&> \case
      Left ratios -> LRatio $ foldl' (+) 0 ratios
      Right ints -> LInt $ foldl' (+) 0 ints

    -- unary should be negation
    isub :: Builtin
    isub args = promote "-" args <&> \case
      Left [x] -> LRatio (-x)
      Left xs -> LRatio $ foldl1' (-) xs
      Right [x] -> LInt (-x)
      Right xs -> LInt $ foldl1' (-) xs

    imul :: Builtin
    imul args = promote "*" args <&> \case
      Left ratios -> LRatio $ foldl' (*) 1 ratios
      Right ints -> LInt $ foldl' (*) 1 ints

    -- unary should be reciprocal
    iidiv :: Builtin
    iidiv args = asInts "div" args >>= \case
      [x] -> pure $ LInt (1 `div` x)
      [] -> numArgsAtLeast "div" 1 []
      xs -> pure $ LInt $ foldl1' div xs

    idiv :: Builtin
    idiv args = traverse (toRatio "/") args >>= \case
      [x] -> pure $ LRatio (1 / x)
      [] -> numArgsAtLeast "/" 1 []
      xs -> pure $ LRatio $ foldl1 (/) xs

    imod, iquot, irem :: Builtin
    imod args = asInts "mod" args >>= \case
      [] -> numArgsAtLeast "mod" 1 []
      xs -> pure $ LInt $ foldl1' mod xs
    iquot args = asInts "quot" args >>= \case
      [] -> numArgsAtLeast "quot" 1 []
      xs -> pure $ LInt $ foldl1' quot xs
    irem args = asInts "rem" args >>= \case
      [] -> numArgsAtLeast "rem" 1 []
      xs -> pure $ LInt $ foldl1' rem xs

    primNumerator :: Builtin
    primNumerator [LInt n] = pure $ LInt n
    primNumerator [LRatio n] = pure $ LInt $ numerator n
    primNumerator [e] = evalError $ "numerator: not a ratio: " <> renderType e
    primNumerator args = numArgs "numerator" 1 args

    primDenominator :: Builtin
    primDenominator [LInt _] = pure $ LInt 1
    primDenominator [LRatio n] = pure $ LInt $ denominator n
    primDenominator [e] = evalError $ "denominator: not a ratio: " <> renderType e
    primDenominator args = numArgs "denominator" 1 args

    comparison name rs is args = promote name args >>= \case
      Left [] -> numArgsAtLeast name 1 []
      Right [] -> numArgsAtLeast name 1 []
      Left xs -> pure $ LBool $ and $ zipWith rs xs (tail xs)
      Right xs -> pure $ LBool $ and $ zipWith is xs (tail xs)

    ieq = comparison "=" (==) (==)
    ine = comparison "/=" (/=) (/=)
    igt = comparison ">" (>) (>)
    ilt = comparison "<" (<) (<)
    ige = comparison ">=" (>=) (>=)
    ile = comparison "<=" (<=) (<=)

    primSet :: Builtin
    primSet [LSymbol name, value] = setVar name value $> value
    primSet [_, _] = evalError "set: expected symbol as variable name"
    primSet args = numArgs "set" 2 args

    primEval :: Builtin
    primEval [e] = eval e
    primEval args = numArgs "eval" 1 args

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

    primNot :: Builtin
    primNot [LBool False] = pure $ LBool True
    primNot [_] = pure $ LBool False
    primNot args = numArgs "not" 1 args

    equal :: Builtin
    equal args =
      case args of
        [x, y] -> LBool <$> equal' x y
        _ -> numArgs "equal" 2 args
      where
        equal' :: Expr -> Expr -> Eval Bool
        equal' (LInt x) (LInt y) = pure (x == y)
        equal' (LRatio x) (LRatio y) = pure (x == y)
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

    typeOf :: Builtin
    typeOf [v] = pure $ LSymbol $ typeToSymbol v
    typeOf args = numArgs "type-of" 1 args

    load :: Builtin
    load [LString path] = do
      contents <- liftIO $ readFile $ Text.unpack path
      case parseFile contents of
        Right res -> progn res
        Left e -> evalError $ "load: parse error: " <> Text.pack e
    load [e] = evalError $ "load: expected string as path, but got " <> renderType e
    load args = numArgs "load" 1 args

    exit :: Builtin
    exit [] = liftIO Exit.exitSuccess
    exit [LInt n] = liftIO $ Exit.exitWith $ Exit.ExitFailure (fromIntegral n)
    exit args = numArgsBound "exit" (0, 1) args

builtinPreds :: [(Symbol, Expr)]
builtinPreds =
  [ ("typep", LBuiltin typep)
  , ("numberp", LBuiltin (typePred "numberp" "number"))
  , ("integerp", LBuiltin (typePred "integerp" "integer"))
  , ("ratiop", LBuiltin (typePred "ratiop" "ratio"))
  , ("rationalp", LBuiltin (typePred "rationalp" "rational"))
  , ("boolp", LBuiltin (typePred "boolp" "bool"))
  , ("charp", LBuiltin (typePred "charp" "char"))
  , ("keywordp", LBuiltin (typePred "keywordp" "keyword"))
  , ("stringp", LBuiltin (typePred "stringp" "string"))
  , ("symbolp", LBuiltin (typePred "symbolp" "symbol"))
  , ("null", LBuiltin (typePred "null" "null"))
  , ("listp", LBuiltin (typePred "listp" "list"))
  , ("consp", LBuiltin (typePred "consp" "cons"))
  , ("functionp", LBuiltin (typePred "functionp" "function"))
  , ("macrop", LBuiltin (typePred "macrop" "macro"))
  ]
  where
    typep' :: Expr -> Symbol -> Eval Bool
    typep' v s =
      case symbolToTypePred s of
        Just p  -> pure $ p v
        Nothing -> evalError "typep: invalid type specified"

    typep :: Builtin
    typep [v, LSymbol s] = LBool <$> typep' v s
    typep [_, _] = evalError "typep: invalid type specified"
    typep args = numArgs "typep" 2 args

    typePred :: Symbol -> Symbol -> Builtin
    typePred name s = \case
      [v] -> LBool <$> typep' v s
      args -> numArgs name 1 args

builtinDefs :: [(Symbol, Expr)]
builtinDefs =
  [ ("nil", nil)
  ]
