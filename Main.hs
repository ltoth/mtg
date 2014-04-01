{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Acid
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import Game.MtG.Acid
import Game.MtG.Types
import Game.MtG.CardTextParser (parseAndSetAbilities)
import Game.MtG.JSONParser (parseSet)

setFile :: String
setFile = "THS.json"

main :: IO ()
main = do
    state <- openLocalState (CardDB Map.empty)
    cs <- getPersistableCardSet setFile
    update state (AddCardSet cs)
    css <- query state GetCardSets
    mapM_ print css

getPersistableCardSet :: FilePath -> IO CardSet
getPersistableCardSet fp = do
    Just cs' <- parseSet fp
    return $ persistableCardSet cs'

getPersistableCards :: FilePath -> IO [Card]
getPersistableCards fp = do
    Just cs' <- parseSet fp
    return $ go (cs'^.code') <$> (cs'^.cards')
    where go c = setCardSetCode c .
                 parseAndSetAbilities

setCardSetCode :: SetCode -> Card -> Card
setCardSetCode sc = setCode .~ sc

persistableCardSet :: CardSet' -> CardSet
persistableCardSet cs' =
    CardSet
      (cs'^.setName')
      (cs'^.code')
      (cs'^.release')
      (cs'^.border')
      (cs'^.setType')
      (cs'^.block')
      (cs'^..cards'.traversed.multiverseID)

getCards :: FilePath -> IO (Maybe [Card])
getCards fp = (view cards' <$>) <$> parseSet fp

filterCards :: (Card -> Bool) -> IO [Card]
filterCards p = go <$> getCards setFile
  where go = fmap parseAndSetAbilities .
             filter p .
             fromMaybe []

cardTextIncludes :: String -> Card -> Bool
cardTextIncludes s = fromMaybe False .
                     (view cardText >=> return . isInfixOf s)

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

