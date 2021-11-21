{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Control.Monad.Catch (MonadMask, MonadCatch, MonadThrow)
import Control.Monad.Except ( ExceptT(ExceptT), MonadError )
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT(..), MonadReader, local, ask)
import Data.CaseInsensitive (CI, foldedCase, mk)
import Data.IORef (IORef, readIORef, newIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ratio qualified as Ratio
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Text qualified as Text
import TextShow (TextShow(..))
import TextShow qualified (fromText, unwordsB, FromTextShow(..))

newtype Symbol = Symbol { getSymbol :: CI Text }
  deriving newtype (Eq, Ord, IsString)

mkSymbol :: Text -> Symbol
mkSymbol = Symbol . mk

fromSymbol :: Symbol -> Text
fromSymbol (Symbol s) = foldedCase s

data Keyword = SymKeyword Symbol | ArbKeyword Text
  deriving (Eq, Ord)

renderKeyword :: Keyword -> Text
renderKeyword = \case
  SymKeyword sym -> ":" <> fromSymbol sym
  ArbKeyword sym -> ":|" <> Text.concatMap escapePipe sym <> "|"
  where
    escapePipe '|' = "\\|"
    escapePipe c = Text.singleton c

data Closure = Closure
  { closureName :: Maybe Symbol
  , closureParams :: [Symbol] -- name
  , closureOptionalParams :: [(Symbol, Expr)] -- name and default value (nil if unspecified)
  , closureRest :: Maybe Symbol -- name
  , closureKeywordParams :: Map Symbol Expr -- name and default value (nil if unspecified)
  , closureBody :: [Expr]
  , closureContext :: IORef Context
  }

data Expr
  = LInt Integer
  | LRatio Rational
  | LBool Bool
  | LChar Char
  | LKeyword Keyword
  | LString Text
  | LSymbol Symbol
  | LList [Expr]
  | LDottedList [Expr] Expr
  | LBuiltin ([Expr] -> Eval Expr)
  | LFun Closure
  | LMacro Closure
  deriving Show via (TextShow.FromTextShow Expr)

renderType :: Expr -> Text
renderType = fromSymbol . typeToSymbol

typeToSymbol :: Expr -> Symbol
typeToSymbol = \case
  LInt _ -> "integer"
  LRatio _ -> "ratio"
  LBool _ -> "bool"
  LChar _ -> "char"
  LKeyword _ -> "keyword"
  LString _ -> "string"
  LSymbol _ -> "symbol"
  LList [] -> "null"
  LList _ -> "cons"
  LDottedList _ _ -> "cons"
  LBuiltin _ -> "function"
  LFun _ -> "function"
  LMacro _ -> "macro"

symbolToTypePred :: Symbol -> Maybe (Expr -> Bool)
symbolToTypePred = \case
  "number" -> pure $ \case { LInt _ -> True; LRatio _ -> True; _ -> False }
  "integer" -> pure $ \case { LInt _ -> True; _ -> False }
  "ratio" -> pure $ \case { LRatio _ -> True; _ -> False }
  "rational" -> pure $ \case { LRatio _ -> True; _ -> False }
  "bool" -> pure $ \case { LBool _ -> True; _ -> False }
  "char" -> pure $ \case { LChar _ -> True; _ -> False }
  "keyword" -> pure $ \case { LKeyword _ -> True; _ -> False }
  "string" -> pure $ \case { LString _ -> True; _ -> False }
  "symbol" -> pure $ \case { LSymbol _ -> True; _ -> False }
  "null" -> pure $ \case { LList [] -> True; _ -> False }
  "list" -> pure $ \case { LList _ -> True; LDottedList _ _ -> True; _ -> False }
  "cons" -> pure $ \case { LDottedList _ _ -> True; LList (_:_) -> True; _ -> False }
  "function" -> pure $ \case { LBuiltin _ -> True; LFun _ -> True; _ -> False }
  "macro" -> pure $ \case { LMacro _ -> True; _ -> False }
  _ -> Nothing

renderChar :: Char -> Text
renderChar = \case
  ' ' -> "#\\space"
  '\t' -> "#\\tab"
  '\n' -> "#\\newline"
  '\r' -> "#\\return"
  c -> "#\\" <> Text.singleton c

renderRatio :: Rational -> Text
renderRatio n = showt num <> "/" <> showt den
  where (num, den) = (Ratio.numerator n, Ratio.denominator n)

instance TextShow Expr where
  showb = \case
    LInt n -> showb n
    LRatio n -> TextShow.fromText $ renderRatio n
    LBool False -> "#f"
    LBool True -> "#t"
    LChar c -> TextShow.fromText $ renderChar c
    LKeyword kw -> TextShow.fromText $ renderKeyword kw
    LString s -> showb s
    LSymbol s -> TextShow.fromText $ fromSymbol s
    LList [LSymbol "quote", x] -> "'" <> showb x
    LList [] -> "nil"
    LList xs -> "(" <> TextShow.unwordsB (fmap showb xs) <> ")"
    LDottedList xs x -> "(" <> TextShow.unwordsB (fmap showb xs) <> " . " <> showb x <> ")"
    LBuiltin _ -> "<builtin>"
    LFun _ -> "<function>"
    LMacro _ -> "<macro>"

-- Evaluation context (scopes)

newtype Error = Error { getError :: Text }

data TagName
  = TagInt Integer
  | TagRatio Rational
  | TagSymbol Symbol
  | TagKeyword Keyword
  deriving (Eq, Ord)

renderTagName :: TagName -> Text
renderTagName = \case
  TagInt n -> showt n
  TagRatio n -> renderRatio n
  TagSymbol sym -> fromSymbol sym
  TagKeyword kw -> renderKeyword kw

data Bubble
  = ReturnFrom Symbol Expr
  | TagGo TagName
  | EvalError Error

newtype Context = Context { getContext :: Map Symbol (IORef Expr) }

-- Right-biased
instance Semigroup Context where
  (Context c1) <> (Context c2) = Context (Map.union c2 c1)
instance Monoid Context where
  mempty = Context Map.empty

-- Eval

newtype Eval a = Eval { runEval :: IORef Context -> IO (Either Bubble a) }
  deriving
    ( Functor, Applicative, Monad
    , MonadReader (IORef Context), MonadError Bubble
    , MonadThrow, MonadCatch, MonadMask
    , MonadIO
    )
    via ReaderT (IORef Context) (ExceptT Bubble IO)

inContext :: IORef Context -> Eval a -> Eval a
inContext ctx' = local (const ctx')

withLocalBindings :: Context -> Eval a -> Eval a
withLocalBindings bindings act = do
  ctx <- liftIO . readIORef =<< ask
  ctx' <- liftIO $ newIORef $ ctx <> bindings
  local (const ctx') act
