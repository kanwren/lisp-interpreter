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
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import TextShow (TextShow(..))

import Errors
import Parser (parseFile)
import Types

nil :: Expr
nil = LList []

mkContext :: [(Symbol, Expr)] -> Eval Context
mkContext pairs = liftIO $ do
  Context . Map.fromList <$> traverse (\(x, y) -> newIORef y <&> (x,)) pairs

evalFile :: String -> Eval Expr
evalFile contents =
  case parseFile contents of
    Right res -> progn res
    Left e -> evalError $ "load: parse error: " <> Text.pack e

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
specialOps = Set.fromList $ specialForms ++ protectedOps
  where
    specialForms =
      [ "quote"
      , "list"
      , "list*"
      , "eval"
      , "if"
      , "and"
      , "or"
      , "the"
      , "setq"
      , "defvar"
      , "defparameter"
      , "defun"
      , "lambda"
      , "let"
      , "defmacro"
      , "flet"
      , "labels"
      , "macrolet"
      , "progn"
      , "when"
      , "unless"
      , "block"
      , "return-from"
      , "go"
      , "tagbody"
      ]
    -- These operations are protected because they are depended on by backquote
    -- splicing, and should not be overridden
    protectedOps =
      [ "cons"
      , "list"
      , "list*"
      , "append"
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
function opName name n args = do
  context <- ask
  functionWithContext context opName name n args

functionWithContext :: IORef Context -> Symbol -> Maybe Symbol -> Int -> [Expr] -> Eval Closure
functionWithContext context opName name n args =
  case args of
    (LList params:body) -> do
      (mainParams, optionals, rest, keywordParams) <- parseArgs params
      pure Closure
        { closureName = name
        , closureParams = mainParams
        , closureOptionalParams = optionals
        , closureRest = rest
        , closureKeywordParams = keywordParams
        , closureBody = body
        , closureContext = context
        }
    (_:_) -> evalError $ fromSymbol opName <> ": invalid parameter list"
    -- if name is given, it was a parameter, so count it in the list
    _ -> numArgsAtLeast opName n $ maybe args (\n' -> LSymbol n':args) name
  where
    toError :: Text -> Eval a
    toError e = evalError $ fromSymbol opName <> ": " <> e
    parseArgs :: [Expr] -> Eval ([Symbol], [(Symbol, Expr)], Maybe Symbol, Map Symbol Expr)
    parseArgs = mainArgs []
    -- Parse the main arguments from the parameter list
    mainArgs :: [Symbol] -> [Expr] -> Eval ([Symbol], [(Symbol, Expr)], Maybe Symbol, Map Symbol Expr)
    mainArgs ma []                         = pure (reverse ma, [], Nothing, mempty)
    mainArgs ma (LSymbol "&optional":spec) = optionalArgs (reverse ma, []) spec
    mainArgs ma (LSymbol "&rest":spec)     = restArgs (reverse ma, []) spec
    mainArgs ma (LSymbol "&key":spec)      = keyArgs (reverse ma, [], Nothing, mempty) spec
    mainArgs ma (LSymbol s:xs)             = mainArgs (s:ma) xs
    mainArgs _  (x:_)                      = toError $ "invalid argument list: invalid parameter " <> showt x
    -- Parse the optional arguments from the parameter list
    optionalArgs :: ([Symbol], [(Symbol, Expr)]) -> [Expr] -> Eval ([Symbol], [(Symbol, Expr)], Maybe Symbol, Map Symbol Expr)
    optionalArgs (ma, oa) []                        = pure (ma, reverse oa, Nothing, mempty)
    optionalArgs _        (LSymbol "&optional":_)   = toError "&optional not allowed here"
    optionalArgs (ma, oa) (LSymbol "&rest":spec)    = restArgs (ma, reverse oa) spec
    optionalArgs (ma, oa) (LSymbol "&key":spec)     = keyArgs (ma, reverse oa, Nothing, mempty) spec
    optionalArgs (ma, oa) (LSymbol s:xs)            = optionalArgs (ma, (s, nil):oa) xs
    optionalArgs (ma, oa) (LList [LSymbol s]:xs)    = optionalArgs (ma, (s, nil):oa) xs
    optionalArgs (ma, oa) (LList [LSymbol s, v]:xs) = eval v >>= \v' -> optionalArgs (ma, (s, v'):oa) xs
    optionalArgs _        (x:_)                     = toError $ "invalid argument list: invalid parameter " <> showt x
    -- Parse the rest argument from the parameter list
    restArgs :: ([Symbol], [(Symbol, Expr)]) -> [Expr] -> Eval ([Symbol], [(Symbol, Expr)], Maybe Symbol, Map Symbol Expr)
    restArgs (ma, oa) []                               = pure (ma, oa, Nothing, mempty)
    restArgs _        [LSymbol "&optional"]            = toError "&optional not allowed here"
    restArgs _        [LSymbol "&rest"]                = toError "&rest not allowed here"
    restArgs (ma, oa) [LSymbol s]                      = pure (ma, oa, Just s, mempty)
    restArgs (ma, oa) (LSymbol s:LSymbol "&key":rest)  = keyArgs (ma, oa, Just s, mempty) rest
    restArgs _        (LSymbol _:x:_)                  = toError $ "unexpected extra argument after rest parameter: " <> showt x
    restArgs _        (x:_)                            = toError $ "invalid argument list: invalid parameter " <> showt x
    -- Parse the key arguments from the parameter list
    keyArgs :: ([Symbol], [(Symbol, Expr)], Maybe Symbol, Map Symbol Expr) -> [Expr] -> Eval ([Symbol], [(Symbol, Expr)], Maybe Symbol, Map Symbol Expr)
    keyArgs (ma, oa, r, ka) []                         = pure (ma, oa, r, ka)
    keyArgs _               (LSymbol "&optional":_)    = toError "&optional not allowed here"
    keyArgs _               (LSymbol "&rest":_)        = toError "&rest not allowed here"
    keyArgs _               (LSymbol "&key":_)         = toError "&key not allowed here"
    keyArgs (ma, oa, r, ka) (LSymbol s:xs)             = keyArgs (ma, oa, r, Map.insert s nil ka) xs
    keyArgs (ma, oa, r, ka) (LList [LSymbol s]:xs)     = keyArgs (ma, oa, r, Map.insert s nil ka) xs
    keyArgs (ma, oa, r, ka) (LList [LSymbol s, v]:xs)  = eval v >>= \v' -> keyArgs (ma, oa, r, Map.insert s v' ka) xs
    keyArgs _               (x:_)                      = toError $ "invalid argument list: invalid parameter " <> showt x

typep :: Symbol -> Expr -> Expr -> Eval Bool
typep name v = go
  where
    go (LSymbol s) =
      case symbolToTypePred s of
        Just p  -> pure $ p v
        Nothing -> evalError $ fromSymbol name <> ": invalid type specifier"
    go (LList (LSymbol "and":spec)) = and <$> traverse go spec
    go (LList (LSymbol "or":spec)) = or <$> traverse go spec
    go (LList (LSymbol "not":spec)) =
      case spec of
        [p] -> not <$> go p
        _ -> evalError $ fromSymbol name <> ": expected exactly 1 argument to not, but got " <> showt (length spec)
    -- TODO: support promotion
    go (LList (LSymbol "integer":spec)) =
      case v of
        LInt n ->
          case spec of
            [LInt lower] -> pure $ lower <= n
            [LList [LInt lower]] -> pure $ lower <= n
            [LInt lower, LInt upper] -> pure $ lower <= n && n <= upper
            _ -> evalError $ fromSymbol name <> ": invalid type specifier: invalid arguments to predicate integer"
        _ -> pure False
    go (LList (LSymbol "rational":spec)) =
      case v of
        LRatio n ->
          case spec of
            [LRatio lower] -> pure $ lower <= n
            [LList [LRatio lower]] -> pure $ lower <= n
            [LRatio lower, LRatio upper] -> pure $ lower <= n && n <= upper
            _ -> evalError $ fromSymbol name <> ": invalid type specifier: invalid arguments to predicate integer"
        _ -> pure False
    go _ = evalError $ fromSymbol name <> ": invalid type specifier"

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

letBody :: Symbol -> [Expr] -> Eval ([Expr], [Expr])
letBody name []              = numArgs name 1 []
letBody _    (LList xs:body) = pure (xs, body)
letBody name (_:_)           = evalError $ fromSymbol name <> ": invalid bindings"

eval :: Expr -> Eval Expr
eval (LSymbol sym) = lookupVar sym
eval (LDottedList xs _) = eval (LList (NonEmpty.toList xs))
eval (LList []) = pure nil
eval (LList (f:args)) =
  case f of
    -- NOTE: this builtin is needed to support quoting; a quoted expression
    -- `*x*` is literally turned into `(quote *x*)`, which should result in *x*
    -- without evaluating it. The renderer simply has a special case for lists
    -- of the form `(list 'quote x)`.
    LSymbol "quote" ->
      case args of
        [x] -> pure x
        _   -> numArgs "quote" 1 args
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
          if x' then go xs else pure $ LBool False
      go args
    LSymbol "or" -> do
      let
        go [] = pure $ LBool False
        go [x] = eval x
        go (x:xs) = do
          x' <- eval x
          if truthy x' then pure x' else go xs
      go args
    LSymbol "the" -> do
      case args of
        [spec, v] -> do
          v' <- eval v
          valid <- typep "the" v' spec
          if valid
            then pure v'
            else evalError $ "the: expected type " <> showt spec <> ", but value " <> showt v' <> " has type " <> renderType v'
        _ -> numArgs "the" 2 args
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
    LSymbol "defun" -> do
      case args of
        (LSymbol sym:spec) -> do
          fun <- LFun <$> function "defun" (Just sym) 2 spec
          setVar sym fun
          pure $ LSymbol sym
        (_:_) -> evalError "defun: expected symbol for function name"
        _ -> numArgs "defun" 2 args
    LSymbol "defmacro" ->
      case args of
        (LSymbol sym:spec) -> do
          fun <- LMacro <$> function "defmacro" (Just sym) 2 spec
          setVar sym fun
          pure $ LSymbol sym
        (_:_) -> evalError "defmacro: expected symbol for macro name"
        _ -> numArgs "defmacro" 2 args
    LSymbol "lambda" -> LFun <$> function "lambda" Nothing 1 args
    LSymbol "let" -> do
      (xs, body) <- letBody "let" args
      let
        getBinding (LList [LSymbol name, val]) = (name,) <$> eval val
        getBinding (LSymbol name) = pure (name, nil)
        getBinding _ = evalError "let: invalid variable specification"
      binds <- mkContext =<< traverse getBinding xs
      withLocalBindings binds $ progn body
    LSymbol "flet" -> do
      (xs, body) <- letBody "flet" args
      let
        getFuncBinding (LList (LSymbol name:params:fbody)) = (name,) . LFun <$> function "flet" (Just name) 2 (params:fbody)
        getFuncBinding _ = evalError "flet: invalid function specification"
      binds <- mkContext =<< traverse getFuncBinding xs
      withLocalBindings binds $ progn body
    LSymbol "labels" -> do
      (xs, body) <- letBody "labels" args
      let
        makeBindings :: IORef Context -> Eval Context
        makeBindings ctx = do
          let
            getFuncBinding (LList (LSymbol name:params:fbody)) = (name,) . LFun <$> functionWithContext ctx "labels" (Just name) 2 (params:fbody)
            getFuncBinding _ = evalError "labels: invalid function specification"
          mkContext =<< traverse getFuncBinding xs
      withRecursiveBindings makeBindings (progn body)
    LSymbol "macrolet" -> do
      (xs, body) <- letBody "macrolet" args
      let
        getFuncBinding (LList (LSymbol name:params:fbody)) = (name,) . LMacro <$> function "macrolet" (Just name) 2 (params:fbody)
        getFuncBinding _ = evalError "macrolet: invalid macro specification"
      binds <- mkContext =<< traverse getFuncBinding xs
      withLocalBindings binds $ progn body
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
    -- NOTE: allowed block names are nil and symbols, nil should behave the same
    -- as 'nil
    LSymbol "block" -> do
      case args of
        (LSymbol blockName:body) -> block blockName $ progn body
        (LList []:body)          -> block "nil" $ progn body
        (_:_)                    -> evalError "block: expected symbol as block name"
        _                        -> numArgsAtLeast "block" 1 args
    LSymbol "return-from" -> do
      case args of
        [LSymbol blockName]      -> throwError $ ReturnFrom blockName nil
        [LSymbol blockName, val] -> throwError . ReturnFrom blockName =<< eval val
        [LList []]               -> throwError $ ReturnFrom "nil" nil
        [LList [], val]          -> throwError . ReturnFrom "nil" =<< eval val
        [_]                      -> evalError "return-from: expected symbol for block name"
        [_, _]                   -> evalError "return-from: expected symbol for block name"
        _                        -> numArgsBound "return-from" (1, 2) args
    -- NOTE: allowed tags are numbers, symbols, keywords, and nil
    LSymbol "go" -> do
      case args of
        [LSymbol sym] -> throwError $ TagGo $ TagSymbol sym
        [LKeyword b]  -> throwError $ TagGo $ TagKeyword b
        [LInt n]      -> throwError $ TagGo $ TagInt n
        [LRatio n]    -> throwError $ TagGo $ TagRatio n
        [LList []]    -> throwError $ TagGo $ TagSymbol "nil"
        [e]           -> evalError $ "go: invalid tag type: " <> renderType e
        _             -> numArgs "go" 1 args
    LSymbol "tagbody" -> do
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
      LBuiltin f' -> f'       =<< traverse eval args
      LFun     f' -> apply f' =<< traverse eval args
      LMacro   m  -> eval     =<< apply m args
      e           -> evalError $ "expected function in call: " <> showt e
-- everything other than a list and a symbol is a self-evaluating expression
eval f = pure f

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
    matchParams' bs []     []          (Just r) ks as
      | null ks = pure $ (r, LList as):bs
      | otherwise = (((r, LList as):bs) ++) <$> matchParamsKeywords ks as
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

