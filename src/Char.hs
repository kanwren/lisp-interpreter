{-# language ImportQualifiedPost #-}
{-# language OverloadedStrings #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}

module Char where

import Data.Text (Text)
import Data.Text qualified as Text
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as MC

renderChar :: Char -> Text
renderChar = \case
  ' '  -> "#\\space"
  '\0' -> "#\\null"
  '\SOH' -> "#\\soh"
  '\STX' -> "#\\stx"
  '\ETX' -> "#\\etx"
  '\EOT' -> "#\\eot"
  '\ENQ' -> "#\\enq"
  '\ACK' -> "#\\ack"
  '\BEL' -> "#\\bell"
  '\b' -> "#\\bs"
  '\t' -> "#\\tab"
  '\v' -> "#\\vt"
  '\f' -> "#\\ff"
  '\r' -> "#\\return"
  '\n' -> "#\\newline"
  '\SO' -> "#\\so"
  '\SI' -> "#\\si"
  '\DLE' -> "#\\dle"
  '\DC1' -> "#\\dc1"
  '\DC2' -> "#\\dc2"
  '\DC3' -> "#\\dc3"
  '\DC4' -> "#\\dc4"
  '\NAK' -> "#\\nak"
  '\SYN' -> "#\\syn"
  '\ETB' -> "#\\etb"
  '\CAN' -> "#\\can"
  '\EM' -> "#\\em"
  '\SUB' -> "#\\sub"
  '\ESC' -> "#\\esc"
  '\FS' -> "#\\fs"
  '\GS' -> "#\\gs"
  '\RS' -> "#\\rs"
  '\US' -> "#\\us"
  c -> "#\\" <> Text.singleton c

parseSpecialChar :: forall v. Ord v => M.Parsec v String Char
parseSpecialChar = M.choice
  [ codes '\0'   ["null", "nul"]
  , codes '\SOH' ["soh"]
  , codes '\STX' ["stx"]
  , codes '\ETX' ["etx"]
  , codes '\EOT' ["eot"]
  , codes '\ENQ' ["enq"]
  , codes '\ACK' ["ack"]
  , codes '\BEL' ["bell", "bel"]
  , codes '\b'   ["bs", "backspace"]
  , codes '\t'   ["tab", "ht"]
  , codes '\v'   ["vt"]
  , codes '\f'   ["ff"]
  , codes '\r'   ["return", "cr"]
  , codes '\n'   ["newline", "nl", "linefeed", "lf"]
  , codes '\SO'  ["so"]
  , codes '\SI'  ["si"]
  , codes '\DLE' ["dle"]
  , codes '\DC1' ["dc1"]
  , codes '\DC2' ["dc2"]
  , codes '\DC3' ["dc3"]
  , codes '\DC4' ["dc4"]
  , codes '\NAK' ["nak"]
  , codes '\SYN' ["syn"]
  , codes '\ETB' ["etb"]
  , codes '\CAN' ["can"]
  , codes '\EM'  ["em"]
  , codes '\SUB' ["sub"]
  , codes '\ESC' ["esc", "escape"]
  , codes '\FS'  ["fs"]
  , codes '\GS'  ["gs"]
  , codes '\RS'  ["rs"]
  , codes '\US'  ["us"]
  , codes ' '    ["space", "sp"]
  ]
  where
    codes :: Char -> [String] -> M.Parsec v String Char
    codes c xs = c <$ M.choice (fmap MC.string' xs)
