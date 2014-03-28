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
import Control.Lens
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (fromMaybe)

import MagicCards

getCards :: FilePath -> IO (Maybe [Card])
getCards fp = (cards <$>) <$> getSet fp

getSet :: FilePath -> IO (Maybe CardSet)
getSet fp = decode <$> L.readFile fp

filterCards :: (Card -> Bool) -> IO [Card]
filterCards p = go <$> getCards setFile
  where go = fmap parseAndSetAbilities .
             filter p .
             fromMaybe []

cardTextIncludes :: String -> Card -> Bool
cardTextIncludes s = fromMaybe False . (view cardText >=> pure . isInfixOf s)

nameStartsWith :: String -> Card -> Bool
nameStartsWith s = isPrefixOf s . view name

effects :: Ability -> [Effect]
effects (ActivatedAbility _ es _) = es
effects (TriggeredAbility _ es _) = es
effects (SpellAbility es) = es
effects _ = []

hasEffect :: (Effect -> Bool) -> [Ability] -> Bool
hasEffect p as = any p (concatMap effects as)

hasOtherEffect :: [Ability] -> Bool
hasOtherEffect = hasEffect isOtherEffect

isOtherEffect :: Effect -> Bool
isOtherEffect (OtherEffect _) = True
isOtherEffect _ = False

isETB :: Effect -> Bool
isETB (OtherEffect e) = "{This} enters the battlefield" `isInfixOf` e
isETB _ = False

hasTEOther :: [Ability] -> Bool
hasTEOther = any isTEOther
  where isTEOther (TriggeredAbility (TEOther _) _ _) = True
        isTEOther _ = False

setFile :: String
setFile = "THS.json"
