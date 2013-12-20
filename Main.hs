{-# LANGUAGE NoMonomorphismRestriction #-}

module Main
( main
, filterCards
, getSet
, getCards
) where

import Control.Applicative
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Maybe (fromMaybe)

import MagicCards

getCards :: FilePath -> IO (Maybe [Card])
getCards fp = getSet fp >>= return . (cards <$>)

debugSet :: IO (Either String CardSet)
debugSet =  eitherDecode <$> L.readFile setFile

getSet :: FilePath -> IO (Maybe CardSet)
getSet fp =  decode <$> L.readFile fp

filterCards :: (Card -> Bool) -> IO [Card]
filterCards p = getCards setFile >>= return . (filter p) . (fromMaybe [])

p1 = (\c -> rarity c == MythicRare && cmc c == Just 5)
p2 = (\c -> R `elem` fromMaybe [] (manaCost c))
p3 = (\c -> Legendary `elem` fromMaybe [] (supertypes c))
foo = map name <$> filterCards p1

setFile = "THS.json"

main = foo
