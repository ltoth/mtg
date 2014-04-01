{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Acid
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (fromMaybe)

import Game.MtG.Acid
import Game.MtG.Types
import Game.MtG.CardTextParser (parseAndSetAbilities)
import Game.MtG.JSONParser (parseSet)

setFile :: String
setFile = "THS.json"

main :: IO ()
main = do
    state <- openLocalState initialCardDB
    -- update state ClearCardDB
    -- persistCardSet state setFile
    css <- query state GetCardSets
    mapM_ print css
    c <- query state (GetCard 373603)
    print c
    closeAcidState state

persistCardSet :: AcidState CardDB -> FilePath -> IO CardSet
persistCardSet st fp = do
    Just cs' <- parseSet fp
    let cs = persistableCardSet cs'
    update st (AddCardSet cs)
    mapM_ (update st . AddCard) (persistableCards cs')
    return cs

persistableCards :: CardSet' -> [Card]
persistableCards cs' = fmap fill (cs'^.cards')
    where fill = parseAndSetAbilities .
                 (setCode .~ (cs'^.code'))

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

debugGetCards :: FilePath -> IO (Maybe [Card])
debugGetCards fp = (view cards' <$>) <$> parseSet fp

filterCards :: (Card -> Bool) -> IO [Card]
filterCards p = go <$> debugGetCards setFile
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

