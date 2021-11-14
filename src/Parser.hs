{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser (parseLine) where

import Control.Arrow (left)
import Control.Monad (void)
import Data.Functor (($>))
import Data.Text qualified as Text
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as MC
import Text.Megaparsec.Char.Lexer qualified as MCL

import Types

type Parser = M.Parsec Void String

space :: Parser ()
space = MCL.space MC.space1 (MCL.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = MCL.lexeme space

symbol :: String -> Parser String
symbol = MCL.symbol space

pExpr :: Parser Expr
pExpr = M.choice
  [ pQuote
  , pList
  , pString
  , try pChar <|> try pInt <|> try pBool <|> pSymbol
  ]
  where
    pInt = LInt <$> MCL.signed (pure ()) MCL.decimal
    pBool = fmap LBool $ (MC.string "#f" $> False) <|> (MC.string "#t" $> True)
    pChar = try pSpecialChar <|> pAnyChar
    pSpecialChar = fmap LChar $ MC.string "#\\" *> M.choice
      [ "space" $> ' '
      , "tab" $> '\t'
      , "newline" $> '\n'
      , "return" $> '\r'
      ]
    pAnyChar = LChar <$> (MC.string "#\\" *> M.anySingle)
    pString = LString . Text.pack <$> (MC.char '"' *> M.manyTill MCL.charLiteral (MC.char '"'))
    pSymbol = do
      let idChar = MC.letterChar <|> M.oneOf ("+-*/!#$%&|:<=>?@^_~" :: String)
      first <- idChar
      rest <- M.many (idChar <|> MC.numberChar)
      pure $ LSymbol $ mkSymbol $ Text.pack $ first:rest
    pQuote = MC.char '\'' *> do
      e <- pExpr
      pure $ LList [LSymbol "quote", e]
    pList = do
      void $ symbol "("
      res <- M.sepEndBy pExpr space
      dotted <- optional $ symbol "." *> lexeme pExpr
      void $ MC.char ')'
      case dotted of
        Nothing -> pure $ LList res
        Just end -> pure $ LDottedList res end

parseLine :: String -> Either String (Maybe Expr)
parseLine input = left errorBundlePretty $ M.parse (space *> optional (lexeme pExpr) <* M.eof) "repl" input
