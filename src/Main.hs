{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Error.Class (catchError)
import Control.Monad.IO.Class
import Control.Monad.Trans (lift)
import Data.Text qualified as Text
import System.Console.Haskeline (InputT, runInputT, defaultSettings, getInputLine)
import Control.Monad.Except (MonadError)
import TextShow (showt)

import Builtins (mkBuiltins)
import Eval (eval)
import Parser (parseLine)
import Types (Error(..), Eval, runEvalWithContext, Bubble(..), fromSymbol)

tryError :: MonadError e m => m a -> m (Either e a)
tryError act = fmap Right act `catchError` (pure . Left)

handleBubble :: MonadIO m => (a -> m ()) -> Either Bubble a -> m ()
handleBubble h = \case
  Left (EvalError (Error e)) -> liftIO $ putStrLn $ Text.unpack e
  Left (ReturnFrom blockName _) -> liftIO $ putStrLn $ Text.unpack $
    "<toplevel>: error returning from block " <> showt (fromSymbol blockName) <> ": no such block in scope"
  Right v -> h v

main :: IO ()
main = do
  builtins <- mkBuiltins
  (res, _) <- flip runEvalWithContext builtins $ runInputT defaultSettings loop
  handleBubble (\_ -> pure ()) res
  where
    loop :: InputT Eval ()
    loop = getInputLine "> " >>= \case
      Nothing -> pure ()
      Just ":q" -> pure ()
      Just line -> do
        -- TODO: resume!
        case parseLine line of
          Left e -> liftIO $ putStrLn e
          Right Nothing -> pure () -- comment line or empty line
          Right (Just expr) -> lift $ do
            tryError (eval expr) >>= handleBubble (liftIO . print)
        loop

