{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Parser (parseLine, parseFile) where

import Control.Arrow (left)
import Control.Monad (void)
import Data.Functor (($>), (<&>))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as MC
import Text.Megaparsec.Char.Lexer qualified as MCL

import Char (parseSpecialChar)
import Types

type Parser = M.Parsec Void String

space :: Parser ()
space = MCL.space MC.space1 (MCL.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = MCL.lexeme space

symbol :: String -> Parser String
symbol = MCL.symbol space

pNumber :: Parser Expr
pNumber = label "number literal" $ do
  num <- MCL.signed (pure ()) MCL.decimal
  denom <- optional $ MC.char '/' *> MCL.decimal
  case denom of
    Just d -> pure $ LRatio $ num % d
    Nothing -> pure $ LInt num

pBool :: Parser Expr
pBool = label "bool literal" $ fmap LBool $ (MC.string "#f" $> False) <|> (MC.string "#t" $> True)

pChar :: Parser Expr
pChar = label "char literal" $ do
  void $ MC.string "#\\"
  let pAnyChar = LChar <$> M.anySingle
  try (LChar <$> parseSpecialChar) <|> pAnyChar <?> "char name"

pKeyword :: Parser Expr
pKeyword = label "keyword" $ MC.char ':' *> M.choice
  [ MC.char '|' *> (LKeyword . ArbKeyword . Text.pack <$> M.manyTill (try (MC.string "\\|" $> '|') <|> MCL.charLiteral) (MC.char '|'))
  , LKeyword . SymKeyword . mkSymbol <$> pIdent -- TODO: this is kinda a hack
  ]

pString :: Parser Expr
pString = label "string literal" $ LString . Text.pack <$> (MC.char '"' *> M.manyTill MCL.charLiteral (MC.char '"'))

pIdent :: Parser Text
pIdent = label "identifier" $ do
  let idChar = MC.letterChar <|> M.oneOf ("+-*/!$%&|:<=>?@^_~." :: String)
        <?> "identifier first char"
  let idTailChar = idChar <|> MC.numberChar <|> M.oneOf ("#" :: String)
        <?> "identifier char"
  first <- idChar
  rest <- do
    if first == '.'
    then M.some idTailChar
    else M.many idTailChar
  pure $ Text.pack $ first:rest

pSymbol :: Parser Expr
pSymbol = label "keyword" $ LSymbol . mkSymbol <$> pIdent

-- 'x is the same as (quote x), where "quote" is a special form that returns
-- its only argument without evaluating it. This is coupled with the
-- corresponding builtin in the `Eval` module.
pQuote :: Parser Expr
pQuote = label "quoted expression" $ symbol "'" *> do
  e <- pExpr
  pure $ LList [LSymbol "quote", e]

pList :: Parser Expr
pList = label "list" $ between (symbol "(") (MC.char ')') $ do
  leading <- M.sepEndBy (try pExpr) space
  case NonEmpty.nonEmpty leading of
    Nothing -> pure $ LList leading
    Just neLeading -> optional (symbol "." *> lexeme pExpr) >>= \case
      Nothing -> pure $ LList leading
      Just (LList xs) -> pure $ LList (leading ++ xs)
      Just end -> pure $ LDottedList neLeading end

pExpr :: Parser Expr
pExpr = M.choice
  [ pQuote
  , pList
  , pString
  , pKeyword
  , pChar
  , pBool
  -- try needed due to signs in numbers, '-5' is a number but '-a' is a symbol
  , try pNumber
  , pSymbol
  , symbol "`" *> pBackquoteExpr
  ]

data Splice = ExprSplice | ListSplice

pSplice :: Parser Splice
pSplice = (symbol ",@" $> ListSplice) <|> (symbol "," $> ExprSplice)

-- An xpression in a backquote
pBackquoteExpr :: Parser Expr
pBackquoteExpr = do
  optional pSplice >>= \case
    Nothing -> pPendingSpliceExpr
    Just ExprSplice -> pExpr
    Just ListSplice -> fail "list splice after backquote is not allowed"
  where
    pPendingSpliceExpr :: Parser Expr
    pPendingSpliceExpr = M.choice
      [ pQuoteBackquoteed
      , pListBackquoteed
      , quote <$> pString
      , quote <$> pKeyword
      , quote <$> pChar
      , quote <$> pBool
      , quote <$> try pNumber
      , quote <$> pSymbol
      , symbol "`" *> pBackquoteExpr -- TODO
      ]
      where
        quote v = LList [LSymbol "quote", v]
        pQuoteBackquoteed = label "quoted backquote-expression" $ symbol "'" *> do
          optional pSplice >>= \case
            Nothing -> quote <$> pPendingSpliceExpr
            Just ExprSplice -> pExpr <&> \e -> LList [LSymbol "list", LList [LSymbol "quote", LSymbol "quote"], e]
            Just ListSplice -> pExpr <&> \e -> LList [LSymbol "cons", LList [LSymbol "quote", LSymbol "quote"], e]
        pListBackquoteed = label "quoted backquote-list" $ do
          void $ symbol "("
          _ <- fail "lists in backquotes have not yet been implemented" -- TODO
          let
            expr' = do
              sp <- optional pSplice
              (sp,) <$> case sp of
                Nothing -> pPendingSpliceExpr
                Just _  -> pExpr
          undefined

parseLine :: String -> Either String (Maybe Expr)
parseLine = left errorBundlePretty . M.parse (space *> optional (lexeme pExpr) <* M.eof) "repl"

pFile :: Parser [Expr]
pFile = space *> M.many (lexeme pExpr) <* M.eof

parseFile :: String -> Either String [Expr]
parseFile = left errorBundlePretty . M.parse pFile "load"

