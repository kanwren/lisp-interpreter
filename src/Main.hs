{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad.Error.Class (catchError)
import Control.Monad.IO.Class
import Control.Monad.Trans (lift)
import Data.Text qualified as Text
import System.Console.Haskeline (InputT, runInputT, defaultSettings, getInputLine)
import Control.Monad.Except (MonadError)

import Builtins (mkBuiltins)
import Eval (eval)
import Parser (parseLine)
import Types (Error(..), Eval, runEvalWithContext)

tryError :: MonadError e m => m a -> m (Either e a)
tryError act = fmap Right act `catchError` (pure . Left)

main :: IO ()
main = do
  builtins <- mkBuiltins
  (res, _) <- flip runEvalWithContext builtins $ runInputT defaultSettings loop
  case res of
    Left (Error e) -> putStrLn $ Text.unpack e
    Right _ -> pure ()
  where
    loop :: InputT Eval ()
    loop = getInputLine "> " >>= \case
      Nothing -> loop
      Just ":q" -> pure ()
      Just line -> do
        -- TODO: resume!
        case parseLine line of
          Left e -> liftIO $ putStrLn e
          Right Nothing -> pure ()
          Right (Just expr) -> lift $ tryError (eval expr) >>= \case
            Right x -> liftIO $ print x
            Left (Error e) -> liftIO $ putStrLn $ Text.unpack e
        loop

