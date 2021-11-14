{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Eval where

import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class
import Control.Monad.State ( gets, get, put )
import Data.Functor ((<&>))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import TextShow (TextShow(..))

import Types
import Utils

nil :: Expr
nil = LList []

mkContext :: [(Symbol, Expr)] -> Eval Context
mkContext pairs = liftIO (Context . Map.fromList <$> traverse (\(x, y) -> newIORef y <&> (x,)) pairs)

lookupVar :: Symbol -> Eval Expr
lookupVar i = do
  ctx <- gets getContext
  case ctx Map.!? i of
    Nothing -> throwError $ Error $ "variable not in scope: " <> fromSymbol i
    Just x  -> liftIO $ readIORef x

setVar :: Symbol -> Expr -> Eval ()
setVar i val = do
  ctx <- gets getContext
  case ctx Map.!? i of
    Nothing -> do
      ref <- liftIO $ newIORef val
      put $ Context $ Map.insert i ref ctx
    Just ref -> liftIO $ writeIORef ref val

function :: Symbol -> Symbol -> Int -> [Expr] -> Eval Expr
function opName name n args = case args of
  (params:body) -> do
    context <- get
    let
      asSymbol :: Expr -> Eval Symbol
      asSymbol (LSymbol s) = pure s
      asSymbol _ = throwError $ Error $ fromSymbol opName <> ": invalid argument list"
    case params of
      LList params' -> do
        params'' <- traverse asSymbol params'
        pure $ LFun Closure
          { closureName = name
          , closureParams = params''
          , closureVarargs = Nothing
          , closureBody = body
          , closureContext = context
          }
      LDottedList params' rest -> do
        params'' <- traverse asSymbol params'
        varargs <- asSymbol rest
        pure $ LFun Closure
          { closureName = name
          , closureParams = params''
          , closureVarargs = Just varargs
          , closureBody = body
          , closureContext = context
          }
      LSymbol rest -> do
        pure $ LFun Closure
          { closureName = name
          , closureParams = []
          , closureVarargs = Just rest
          , closureBody = body
          , closureContext = context
          }
      _ -> throwError $ Error $ fromSymbol opName <> ": invalid argument list"
  _ -> numArgs opName n args

progn :: [Expr] -> Eval Expr
progn [] = pure nil
progn [x] = eval x
progn (x:y) = eval x *> progn y

truthy :: Expr -> Bool
truthy = \case
  LBool b -> b
  _ -> True

condition :: Expr -> Eval Bool
condition cond = truthy <$> eval cond

eval :: Expr -> Eval Expr
eval (LInt n) = pure $ LInt n
eval (LBool b) = pure $ LBool b
eval (LChar b) = pure $ LChar b
eval (LKeyword b) = pure $ LKeyword b
eval (LString s) = pure $ LString s
eval (LFun f) = pure $ LFun f
eval (LBuiltin f) = pure $ LBuiltin f
eval (LSymbol sym) = lookupVar sym
eval (LDottedList xs _) = eval (LList xs)
eval (LList []) = pure $ LList []
eval (LList (f:args)) =
  case f of
    LSymbol "if" -> do
      case args of
        [cond, x, y] -> do
          cond' <- condition cond
          if cond' then eval x else eval y
        _ -> numArgs "if" 3 args
    LSymbol "and" -> do
      let
        go [] = pure $ LBool True
        go [x] = pure x
        go (x:xs) = do
          x' <- condition x
          if x'
          then go xs
          else pure $ LBool False
      go args
    LSymbol "or" -> do
      let
        go [] = pure $ LBool False
        go [x] = pure x
        go (x:xs) = do
          x' <- eval x
          if truthy x'
          then pure x'
          else go xs
      go args
    LSymbol "set" -> do
      case args of
        [name, val] -> do
          eval name >>= \case
            LSymbol name' -> do
              val' <- eval val
              setVar name' val'
              pure val'
            _ -> throwError $ Error "set: expected symbol as variable name"
        _ -> numArgs "set" 2 args
    LSymbol "setq" -> do
      case args of
        [LSymbol e, val] -> do
          val' <- eval val
          setVar e val'
          pure val'
        [_, _] -> throwError $ Error "setq: expected symbol as variable name"
        _ -> numArgs "setq" 2 args
    LSymbol "eval" ->
      case args of
        [e] -> eval e
        _ -> numArgs "eval" 1 args
    LSymbol "quote" ->
      case args of
        [x] -> pure x
        _ -> numArgs "quote" 1 args
    LSymbol "list" -> LList <$> traverse eval args
    LSymbol "defun" -> do
      case args of
        (LSymbol sym:spec) -> do
          fun <- function "defun" sym 2 spec
          setVar sym fun
          pure $ LSymbol sym
        (_:_) -> throwError $ Error "defun: expected symbol for function name"
        _ -> numArgs "defun" 2 args
    LSymbol "lambda" -> function "lambda" "<lambda>" 1 args
    LSymbol "let" -> do
      case args of
        [] -> numArgs "let" 1 args
        (LList xs:body) -> do
          let
            getBinding (LList [LSymbol name, val]) = pure (name, val)
            getBinding (LSymbol name) = pure (name, nil)
            getBinding _ = throwError $ Error "let: invalid variable specification"
          binds <- mkContext =<< traverse getBinding xs
          localContext (<> binds) $ do
            progn body
        _ -> throwError $ Error "let: invalid variable specification"
    LSymbol "defmacro" -> undefined
    LSymbol "progn" -> progn args
    LSymbol "when" -> do
      case args of
        (cond:body) -> do
          cond' <- condition cond
          if cond' then progn body else pure nil
        _ -> numArgsAtLeast "when" 1 args
    LSymbol "unless" -> do
      case args of
        (cond:body) -> do
          cond' <- condition cond
          if not cond' then progn body else pure nil
        _ -> numArgsAtLeast "unless" 1 args
    _ -> do
      eval f >>= \case
        LBuiltin f' -> do
          args' <- traverse eval args
          f' args'
        LFun f' -> do
          args' <- traverse eval args
          let got = length args'
              expected = length $ closureParams f'
          binds <- case closureVarargs f' of
            Nothing
              | got /= expected -> numArgs (closureName f') expected args'
              | otherwise -> mkContext (zip (closureParams f') args')
            Just rest
              | got < expected -> numArgsAtLeast (closureName f') expected args'
              | otherwise -> do
                  let (mainArgs, restArgs) = splitAt expected args'
                  mkContext (zip (closureParams f') mainArgs <> [(rest, LList restArgs)])
          localContext (\c -> closureContext f' <> c <> binds) $ do
            progn (closureBody f')
        e -> throwError $ Error $ "expected function in s-expr: " <> showt e

-- Builtins

