{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser (parseLine, parseFile) where

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
import Data.Ratio ((%))

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
  , pKeyword
  , pChar
  , pBool
  -- try needed due to signs in numbers, '-5' is a number but '-a' is a symbol
  , try pNumber
  , pSymbol
  ]
  where
    pNumber = label "number literal" $ do
      num <- MCL.signed (pure ()) MCL.decimal
      denom <- optional $ MC.char '/' *> MCL.decimal
      case denom of
        Just d -> pure $ LRatio $ num % d
        Nothing -> pure $ LInt num
    pBool = label "bool literal" $ fmap LBool $ (MC.string "#f" $> False) <|> (MC.string "#t" $> True)
    pChar = label "char literal" $ do
      void $ MC.string "#\\"
      try pSpecialChar <|> pAnyChar <?> "char name"
    pSpecialChar = LChar <$> M.choice
      [ "space" $> ' '
      , "tab" $> '\t'
      , "newline" $> '\n'
      , "return" $> '\r'
      ]
    pAnyChar = LChar <$> M.anySingle
    pKeyword = label "keyword" $ MC.char ':' *> M.choice
      [ MC.char '|' *> (LKeyword . ArbKeyword . Text.pack <$> M.manyTill (try (MC.string "\\|" $> '|') <|> MCL.charLiteral) (MC.char '|'))
      , LKeyword . SymKeyword . mkSymbol <$> pIdent -- TODO: this is kinda a hack
      ]
    pString = label "string literal" $ LString . Text.pack <$> (MC.char '"' *> M.manyTill MCL.charLiteral (MC.char '"'))
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
    pSymbol = label "keyword" $ LSymbol . mkSymbol <$> pIdent
    pQuote = label "quoted expression" $ MC.char '\'' *> do
      e <- pExpr
      pure $ LList [LSymbol "quote", e]
    pList = label "list" $ do
      void $ symbol "("
      res <- M.sepEndBy (try pExpr) space
      dotted <- optional $ symbol "." *> lexeme pExpr
      void $ MC.char ')'
      case dotted of
        Nothing -> pure $ LList res
        Just end -> pure $ LDottedList res end

parseLine :: String -> Either String (Maybe Expr)
parseLine = left errorBundlePretty . M.parse (space *> optional (lexeme pExpr) <* M.eof) "repl"

pFile :: Parser [Expr]
pFile = space *> M.many (lexeme pExpr) <* M.eof

parseFile :: String -> Either String [Expr]
parseFile = left errorBundlePretty . M.parse pFile "load"

