{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Eval where

import Control.Monad (unless)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.IO.Class
import Control.Monad.Reader (ask)
import Data.Foldable (foldlM)
import Data.Functor ((<&>))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import TextShow (TextShow(..))

import Errors
import Types

nil :: Expr
nil = LList []

mkContext :: [(Symbol, Expr)] -> Eval Context
mkContext pairs = liftIO $ do
  Context . Map.fromList <$> traverse (\(x, y) -> newIORef y <&> (x,)) pairs

hasVar :: Symbol -> Eval Bool
hasVar i = do
  Context ctx <- liftIO . readIORef =<< ask
  pure $ i `Map.member` ctx

lookupVar :: Symbol -> Eval Expr
lookupVar i = do
  Context ctx <- liftIO . readIORef =<< ask
  case ctx Map.!? i of
    Nothing -> evalError $ "variable not in scope: " <> fromSymbol i
    Just x  -> liftIO $ readIORef x

specialOps :: Set Symbol
specialOps = Set.fromList
  [ "if"
  , "and"
  , "or"
  , "setq"
  , "defvar"
  , "defparameter"
  , "quote"
  , "defun"
  , "lambda"
  , "let"
  , "defmacro"
  , "progn"
  , "when"
  , "unless"
  , "block"
  , "return-from"
  , "go"
  , "tagbody"
  ]

setVar :: Symbol -> Expr -> Eval ()
setVar i val
  | i `Set.member` specialOps = evalError $ "error: " <> fromSymbol i <> " is a special operator and may not be overridden"
  | otherwise = do
    ctxVar <- ask
    Context ctx <- liftIO $ readIORef ctxVar
    case ctx Map.!? i of
      Nothing -> do
        ref <- liftIO $ newIORef val
        liftIO $ writeIORef ctxVar $ Context $ Map.insert i ref ctx
      Just ref -> liftIO $ writeIORef ref val

function :: Symbol -> Maybe Symbol -> Int -> [Expr] -> Eval Closure
function opName name n args =
  case args of
    (LList params:body) -> do
      context <- ask
      case parseArgs params of
        Right (mainParams, optionals, rest, keywordParams) -> pure Closure
          { closureName = name
          , closureParams = mainParams
          , closureOptionalParams = optionals
          , closureRest = rest
          , closureKeywordParams = keywordParams
          , closureBody = body
          , closureContext = context
          }
        Left e -> throwError $ EvalError e
    (_:_) -> evalError $ fromSymbol opName <> ": invalid parameter list"
    _ -> numArgsAtLeast opName n args
  where
    toError :: Text -> Error
    toError e = Error $ fromSymbol opName <> ": " <> e
    parseArgs :: [Expr] -> Either Error ([Symbol], [(Symbol, Expr)], Maybe Symbol, Map Symbol Expr)
    parseArgs = mainArgs []
    -- Parse the main arguments from the parameter list
    mainArgs :: [Symbol] -> [Expr] -> Either Error ([Symbol], [(Symbol, Expr)], Maybe Symbol, Map Symbol Expr)
    mainArgs ma []                         = pure (reverse ma, [], Nothing, mempty)
    mainArgs ma (LSymbol "&optional":spec) = optionalArgs (reverse ma, []) spec
    mainArgs ma (LSymbol "&rest":spec)     = restArgs (reverse ma, []) spec
    mainArgs ma (LSymbol "&key":spec)      = keyArgs (reverse ma, [], Nothing, mempty) spec
    mainArgs ma (LSymbol s:xs)             = mainArgs (s:ma) xs
    mainArgs _  (x:_)                      = Left $ toError $ "invalid argument list: invalid parameter " <> showt x
    -- Parse the optional arguments from the parameter list
    optionalArgs :: ([Symbol], [(Symbol, Expr)]) -> [Expr] -> Either Error ([Symbol], [(Symbol, Expr)], Maybe Symbol, Map Symbol Expr)
    optionalArgs (ma, oa) []                        = pure (ma, reverse oa, Nothing, mempty)
    optionalArgs _        (LSymbol "&optional":_)   = Left $ toError "&optional not allowed here"
    optionalArgs (ma, oa) (LSymbol "&rest":spec)    = restArgs (ma, reverse oa) spec
    optionalArgs (ma, oa) (LSymbol "&key":spec)     = keyArgs (ma, reverse oa, Nothing, mempty) spec
    optionalArgs (ma, oa) (LSymbol s:xs)            = optionalArgs (ma, (s, nil):oa) xs
    optionalArgs (ma, oa) (LList [LSymbol s]:xs)    = optionalArgs (ma, (s, nil):oa) xs
    optionalArgs (ma, oa) (LList [LSymbol s, v]:xs) = optionalArgs (ma, (s, v):oa) xs
    optionalArgs _        (x:_)                     = Left $ toError $ "invalid argument list: invalid parameter " <> showt x
    -- Parse the rest argument from the parameter list
    restArgs :: ([Symbol], [(Symbol, Expr)]) -> [Expr] -> Either Error ([Symbol], [(Symbol, Expr)], Maybe Symbol, Map Symbol Expr)
    restArgs (ma, oa) []                               = pure (ma, oa, Nothing, mempty)
    restArgs _        [LSymbol "&optional"]            = Left $ toError "&optional not allowed here"
    restArgs _        [LSymbol "&rest"]                = Left $ toError "&rest not allowed here"
    restArgs (ma, oa) [LSymbol s]                      = pure (ma, oa, Just s, mempty)
    restArgs (ma, oa) (LSymbol s:LSymbol "&key":rest)  = keyArgs (ma, oa, Just s, mempty) rest
    restArgs _        (LSymbol _:x:_)                  = Left $ toError $ "unexpected extra argument after rest parameter: " <> showt x
    restArgs _        (x:_)                            = Left $ toError $ "invalid argument list: invalid parameter " <> showt x
    -- Parse the key arguments from the parameter list
    keyArgs :: ([Symbol], [(Symbol, Expr)], Maybe Symbol, Map Symbol Expr) -> [Expr] -> Either Error ([Symbol], [(Symbol, Expr)], Maybe Symbol, Map Symbol Expr)
    keyArgs (ma, oa, r, ka) []                         = pure (ma, oa, r, ka)
    keyArgs _               (LSymbol "&optional":_)    = Left $ toError "&optional not allowed here"
    keyArgs _               (LSymbol "&rest":_)        = Left $ toError "&rest not allowed here"
    keyArgs _               (LSymbol "&key":_)         = Left $ toError "&key not allowed here"
    keyArgs (ma, oa, r, ka) (LSymbol s:xs)             = keyArgs (ma, oa, r, Map.insert s nil ka) xs
    keyArgs (ma, oa, r, ka) (LList [LSymbol s]:xs)     = keyArgs (ma, oa, r, Map.insert s nil ka) xs
    keyArgs (ma, oa, r, ka) (LList [LSymbol s, v]:xs)  = keyArgs (ma, oa, r, Map.insert s v ka) xs
    keyArgs _               (x:_)                      = Left $ toError $ "invalid argument list: invalid parameter " <> showt x

-- Evaluate a list of expressions and return the value of the final expression
progn :: [Expr] -> Eval Expr
progn [] = pure nil
progn [x] = eval x
progn (x:y) = eval x *> progn y

buildTagTable :: [Expr] -> Eval (Map TagName Int, Vector Expr)
buildTagTable = fmap collect . foldlM go (0, mempty, mempty)
  where
    collect (_, tagTable, exprs) = (tagTable, Vector.fromList (reverse exprs))
    go :: (Int, Map TagName Int, [Expr]) -> Expr -> Eval (Int, Map TagName Int, [Expr])
    go (i, tagTable, exprs) = \case
      -- normal symbols
      LSymbol sym -> pure (i, Map.insert (TagSymbol sym) i tagTable, exprs)
      LKeyword b -> pure (i, Map.insert (TagKeyword b) i tagTable, exprs)
      LInt n -> pure (i, Map.insert (TagInt n) i tagTable, exprs)
      -- NIL symbol
      LList [] -> pure (i, Map.insert (TagSymbol "nil") i tagTable, exprs)
      -- lists
      l@(LList _) -> pure (i + 1, tagTable, l:exprs)
      l@(LDottedList _ _) -> pure (i + 1, tagTable, l:exprs)
      e -> evalError $ "tagbody: invalid tag or form type: " <> renderType e

block :: Symbol -> Eval Expr -> Eval Expr
block blockName a =
  a `catchError` \case
    ReturnFrom target val
      | blockName == target -> pure val
      -- NOTE: non-matching block names should bubble up
    e -> throwError e

truthy :: Expr -> Bool
truthy = \case
  LBool b -> b
  _ -> True

condition :: Expr -> Eval Bool
condition cond = truthy <$> eval cond

eval :: Expr -> Eval Expr
eval (LInt n) = pure $ LInt n
eval (LRatio n) = pure $ LRatio n
eval (LBool b) = pure $ LBool b
eval (LChar b) = pure $ LChar b
eval (LKeyword b) = pure $ LKeyword b
eval (LString s) = pure $ LString s
eval (LFun f) = pure $ LFun f
eval (LMacro f) = pure $ LMacro f
eval (LBuiltin f) = pure $ LBuiltin f
eval (LSymbol sym) = lookupVar sym
eval (LDottedList xs _) = eval (LList xs)
eval (LList []) = pure nil
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
        go [x] = eval x
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
    -- NOTE: could be (defmacro setq (name val) (list 'set (list 'quote name) val)
    LSymbol "setq" -> do
      case args of
        [LSymbol e, val] -> do
          val' <- eval val
          setVar e val'
          pure val'
        [_, _] -> evalError "setq: expected symbol as variable name"
        _ -> numArgs "setq" 2 args
    LSymbol "defvar" -> do
      case args of
        [LSymbol e, val] -> do
          exists <- hasVar e
          unless exists $ setVar e =<< eval val
          pure $ LSymbol e
        [_, _] -> evalError "defvar: expected symbol as variable name"
        _ -> numArgs "defvar" 2 args
    LSymbol "defparameter" -> do
      case args of
        [LSymbol e, val] -> do
          setVar e =<< eval val
          pure $ LSymbol e
        [_, _] -> evalError "defvar: expected symbol as variable name"
        _ -> numArgs "defvar" 2 args
    -- NOTE: equivalent to (defmacro quote (x) (list 'quote x))
    LSymbol "quote" ->
      case args of
        [x] -> pure x
        _ -> numArgs "quote" 1 args
    LSymbol "defun" -> do
      case args of
        (LSymbol sym:spec) -> do
          fun <- LFun <$> function "defun" (Just sym) 2 spec
          setVar sym fun
          pure $ LSymbol sym
        (_:_) -> evalError "defun: expected symbol for function name"
        _ -> numArgs "defun" 2 args
    LSymbol "lambda" -> LFun <$> function "lambda" Nothing 1 args
    LSymbol "let" -> do
      case args of
        [] -> numArgs "let" 1 args
        (LList xs:body) -> do
          let
            getBinding (LList [LSymbol name, val]) = (name,) <$> eval val
            getBinding (LSymbol name) = pure (name, nil)
            getBinding _ = evalError "let: invalid variable specification"
          binds <- mkContext =<< traverse getBinding xs
          withLocalBindings binds $ progn body
        _ -> evalError "let: invalid variable specification"
    LSymbol "defmacro" ->
      case args of
        (LSymbol sym:spec) -> do
          fun <- LMacro <$> function "defmacro" (Just sym) 2 spec
          setVar sym fun
          pure $ LSymbol sym
        (_:_) -> evalError "defmacro: expected symbol for macro name"
        _ -> numArgs "defmacro" 2 args
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
    -- NOTE: allowed block names are nil and symbols
    LSymbol "block" -> do
      case args of
        (LSymbol blockName:body) -> block blockName $ progn body
        (LList []:body) -> block "nil" $ progn body
        (_:_) -> evalError "block: expected symbol as block name"
        _ -> numArgsAtLeast "block" 1 args
    LSymbol "return-from" -> do
      case args of
        [LSymbol blockName] -> throwError $ ReturnFrom blockName nil
        [LSymbol blockName, val] -> throwError $ ReturnFrom blockName val
        -- TODO: this should be generalized; 'nil and () are the same in CL
        [LList []] -> throwError $ ReturnFrom "nil" nil
        [LList [], val] -> throwError $ ReturnFrom "nil" val
        [_] -> evalError "return-from: expected symbol for block name"
        [_, _] -> evalError "return-from: expected symbol for block name"
        _ -> numArgsBound "return-from" (1, 2) args
    LSymbol "go" -> do
      case args of
        [LSymbol sym] -> throwError $ TagGo $ TagSymbol sym
        [LKeyword b] -> throwError $ TagGo $ TagKeyword b
        [LInt n] -> throwError $ TagGo $ TagInt n
        [LRatio n] -> throwError $ TagGo $ TagRatio n
        [LList []] -> throwError $ TagGo $ TagSymbol "nil"
        [e] -> evalError $ "go: invalid tag type: " <> renderType e
        _ -> numArgs "go" 1 args
    -- NOTE: allowed tags are numbers, symbols, keywords, and nil
    LSymbol "tagbody" -> do
      -- collect arguments into vectors, construct table mapping symbols to
      -- indices
      -- (watch out for symbols on end and consecutive symbols)
      -- evaluate progn at index 0
      -- catch any TagGo exceptions, and if there's an appropriate tag name in
      -- scope, start evaluating there
      (table, exprs) <- buildTagTable args
      let
        len = Vector.length exprs
        runAt !ix =
          if ix < 0 || ix >= len
          then pure ()
          else eval (exprs Vector.! ix) *> runAt (ix + 1)
        go !ix =
          runAt ix `catchError` \case
            TagGo tagName
              | Just ix' <- table Map.!? tagName -> go ix'
            e -> throwError e
      go 0
      pure nil
    _ -> eval f >>= \case
      LBuiltin f' -> f' =<< traverse eval args
      LFun f' -> apply f' =<< traverse eval args
      LMacro m -> eval =<< apply m args
      e -> evalError $ "expected function in call: " <> showt e

apply :: Closure -> [Expr] -> Eval Expr
apply Closure{..} args = do
  binds <- mkContext =<< matchParams closureParams closureOptionalParams closureRest closureKeywordParams args
  inContext closureContext $ withLocalBindings binds $ do
    progn closureBody `catchError` \case
      -- all (return-from)s need to be caught, or else we could bubble out of
      -- the current function! this is contrasted with `block`, which should
      -- let any (return-from)s with a different label to bubble up
      ReturnFrom blockName val
        | Just blockName == closureName -> pure val
        | otherwise -> evalError $ fromSymbol name <> ": error returning from block " <> showt (fromSymbol blockName) <> ": no such block in scope"
      TagGo tagName -> evalError $ fromSymbol name <> ": error going to tag " <> renderTagName tagName <> ": no such tag in scope"
      e -> throwError e
  where
    minLen = length closureParams
    name = fromMaybe "<anonymous>" closureName
    argsError =
      if isJust closureRest
      then numArgsAtLeast name minLen args
      else numArgsBound name (minLen, minLen + length closureOptionalParams) args
    matchParams :: [Symbol] -> [(Symbol, Expr)] -> Maybe Symbol -> Map Symbol Expr -> [Expr] -> Eval [(Symbol, Expr)]
    matchParams ps os r ks as = reverse <$> matchParams' [] ps os r ks as
    matchParams' :: [(Symbol, Expr)] -> [Symbol] -> [(Symbol, Expr)] -> Maybe Symbol -> Map Symbol Expr -> [Expr] -> Eval [(Symbol, Expr)]
    matchParams' bs (m:ms) os          r        ks (a:as) = matchParams' ((m,a):bs) ms os r ks as
    matchParams' _  (_:_)  _           _        _  []     = argsError -- not enough args
    matchParams' bs []     ((o, _):os) r        ks (a:as) = matchParams' ((o,a):bs) [] os r ks as
    matchParams' bs []     ((o, v):os) r        ks []     = matchParams' ((o, v):bs) [] os r ks []
    matchParams' bs []     []          (Just r) ks as     = (((r, LList as):bs) ++) <$> matchParamsKeywords ks as
    matchParams' bs []     []          Nothing  ks as
      | not (null as) && null ks = argsError -- better error messages when no keyword args and too many parameters
      | otherwise = (bs ++) <$> matchParamsKeywords ks as
    -- TODO: match keywords, generate bindings for everything in map, error if
    -- there are just too many parameters (function calls with too many
    -- parameters get passed here)
    matchParamsKeywords :: Map Symbol Expr -> [Expr] -> Eval [(Symbol, Expr)]
    matchParamsKeywords = go
      where
        go res [] = pure $ Map.assocs res
        go res (LKeyword k@(SymKeyword s):v:rest)
          | s `Map.member` res = go (Map.insert s v res) rest
          | otherwise = evalError $ fromSymbol name <> ": unexpected keyword: " <> renderKeyword k
        go _ (LKeyword _:_) = evalError $ fromSymbol name <> ": expected value for keyword argument"
        go _ (x:_) = evalError $ fromSymbol name <> ": unexpected parameter in keyword arguments: " <> showt x

