{-# LANGUAGE NoMonomorphismRestriction #-}

module Main
( filterCards
, nameStartsWith
, cardTextIncludes
, hasEffect
, hasOtherEffect
, isETB
, hasTEOther
, getSet
, getCards
) where

import Control.Applicative
import Control.Monad ((>=>))
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List (isInfixOf, isPrefixOf)
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

cardTextIncludes s = fromMaybe False . (cardText >=> pure . isInfixOf s)

nameStartsWith s = isPrefixOf s . name

effects (ActivatedAbility _ es _) = es
effects (TriggeredAbility _ es _) = es
effects (SpellAbility es) = es
effects _ = []

hasEffect p as = any p (concatMap effects as)
hasOtherEffect = hasEffect isOtherEffect

isOtherEffect (OtherEffect _) = True
isOtherEffect _ = False

isETB (OtherEffect e) = "{This} enters the battlefield" `isInfixOf` e
isETB _ = False

hasTEOther = any isTEOther
  where isTEOther (TriggeredAbility (TEOther _) _ _) = True
        isTEOther _ = False

setFile = "THS.json"
