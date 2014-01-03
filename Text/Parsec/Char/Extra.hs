module Text.Parsec.Char.Extra
( ciChar
, ciString
) where

import Control.Applicative
import Data.Char (toLower, toUpper)
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

-- http://bit.ly/1bQVGFB
ciChar c = char (toLower c) <|> char (toUpper c)
ciString s = try (mapM ciChar s) <?> "\"" ++ s ++ "\""
