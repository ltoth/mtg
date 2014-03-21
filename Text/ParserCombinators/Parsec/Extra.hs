module Text.ParserCombinators.Parsec.Extra
( sepBy2
, many2
) where

import Text.ParserCombinators.Parsec

sepBy2 seg sep = do
        x <- seg
        sep
        xs <- sepBy1 seg sep
        return (x:xs)

many2 seg = do
    x <- seg
    xs <- many1 seg
    return (x:xs)
