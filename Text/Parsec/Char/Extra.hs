{-# LANGUAGE FlexibleContexts #-}

module Text.Parsec.Char.Extra
( ciChar
, ciString
) where

import Data.Char (toLower, toUpper)
import Text.Parsec.Prim
import Text.Parsec.Char

-- http://bit.ly/1bQVGFB
ciChar :: Stream s m Char => Char -> ParsecT s u m Char
ciChar c = char (toLower c) <|> char (toUpper c)

ciString :: Stream s m Char => String -> ParsecT s u m String
ciString s = try (mapM ciChar s) <?> "\"" ++ s ++ "\""
