module Text.ParserCombinators.Parsec.Extra
( sepBy2
, many2
) where

import Text.Parsec.Prim
import Text.ParserCombinators.Parsec

sepBy2 :: Stream s m t => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepBy2 seg sep = do
        x <- seg
        _ <- sep
        xs <- sepBy1 seg sep
        return (x:xs)

many2 :: Stream s m t => ParsecT s u m a -> ParsecT s u m [a]
many2 seg = do
    x <- seg
    xs <- many1 seg
    return (x:xs)
