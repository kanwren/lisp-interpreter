{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Control.Monad.Catch (MonadMask, MonadCatch, MonadThrow)
import Control.Monad.Except ( ExceptT (ExceptT), runExceptT, MonadError )
import Control.Monad.IO.Class
import Control.Monad.State ( MonadState )
import Control.Monad.State.Strict (StateT (StateT), runStateT)
import Data.CaseInsensitive (CI, foldedCase, mk)
import Data.IORef (IORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.String (IsString(..))
import Data.Text (Text)
import TextShow (TextShow(..))
import TextShow qualified (fromText, unwordsB, FromTextShow(..))

newtype Symbol = Symbol { getSymbol :: CI Text }
  deriving newtype (Eq, Ord, IsString)

mkSymbol :: Text -> Symbol
mkSymbol = Symbol . mk

fromSymbol :: Symbol -> Text
fromSymbol (Symbol s) = foldedCase s

data Closure = Closure
  { closureName :: Symbol
  , closureParams :: [Symbol]
  , closureVarargs :: Maybe Symbol
  , closureBody :: [Expr]
  , closureContext :: Context
  }

data Expr
  = LInt Integer
  | LBool Bool
  | LString Text
  | LSymbol Symbol
  | LList [Expr]
  | LDottedList [Expr] Expr
  | LBuiltin ([Expr] -> Eval Expr)
  | LFun Closure
  deriving Show via (TextShow.FromTextShow Expr)

renderType :: Expr -> Text
renderType = \case
  LInt _ -> "int"
  LBool _ -> "bool"
  LString _ -> "string"
  LSymbol _ -> "symbol"
  LList [] -> "nil"
  LList _ -> "cons"
  LDottedList _ _ -> "cons"
  LBuiltin _ -> "function"
  LFun _ -> "function"

instance TextShow Expr where
  showb = \case
    LInt n -> showb n
    LBool False -> "#f"
    LBool True -> "#t"
    LString s -> showb s
    LSymbol s -> TextShow.fromText $ fromSymbol s
    LList [LSymbol "quote", x] -> "'" <> showb x
    LList [] -> "nil"
    LList xs -> "(" <> TextShow.unwordsB (fmap showb xs) <> ")"
    LDottedList xs x -> "(" <> TextShow.unwordsB (fmap showb xs) <> " . " <> showb x <> ")"
    LBuiltin _ -> "<builtin>"
    LFun _ -> "<function>"

-- Evaluation context (scopes)

newtype Error = Error { getError :: Text }

newtype Context = Context { getContext :: Map Symbol (IORef Expr) }

-- Right-biased
instance Semigroup Context where
  (Context c1) <> (Context c2) = Context (Map.union c2 c1)
instance Monoid Context where
  mempty = Context Map.empty

-- Eval

newtype Eval a = Eval { getEval :: ExceptT Error (StateT Context IO) a }
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadState Context, MonadError Error)
  deriving newtype (MonadThrow, MonadCatch, MonadMask)
  deriving newtype (MonadIO)

runEvalWithContext :: Eval a -> Context -> IO (Either Error a, Context)
runEvalWithContext (Eval x) ctx = flip runStateT ctx $ runExceptT x

localContext :: (Context -> Context) -> Eval a -> Eval a
localContext f act = Eval $ ExceptT $ StateT $ \ctx -> runEvalWithContext act (f ctx)