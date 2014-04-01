{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Control.Applicative
import Control.Lens hiding (argument)
import Control.Monad
import Data.Acid
import Data.Acid.Advanced
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (fromMaybe)
import qualified IPPrint
import qualified Language.Haskell.HsColour as HsColour
import qualified Language.Haskell.HsColour.Colourise as HsColour
import qualified Language.Haskell.HsColour.Output as HsColour
import Options.Applicative

import Game.MtG.Acid
import Game.MtG.Types
import Game.MtG.CardTextParser (parseAndSetAbilities)
import Game.MtG.JSONParser (parseSet)

setFile :: String
setFile = "THS.json"

opts :: Parser (IO ())
opts = subparser
   ( command "clear"   (info (pure clearCmd)
                       (progDesc "Clear the card database") )
  <> command "card"    (info (cardCmd <$>
                              argument auto (metavar "ID"))
                       (progDesc "Get parsed card by multiverseID") )
  <> command "name"    (info (nameCmd <$>
                              argument str (metavar "NAME"))
                       (progDesc "Get parsed cards by exact name") )
  <> command "names"   (info (pure namesCmd)
                       (progDesc "Get all parsed card names") )
  <> command "sets"    (info (pure setsCmd)
                       (progDesc "Get all parsed sets") )
  <> command "persist" (info (persistCmd <$>
                              argument str (metavar "FILE"))
                       (progDesc "Parse set and cards and persist them") )
   )

main :: IO ()
main = join $ execParser (info (helper <*> opts)
                         (header "mtg - a parser for M:tG" <> fullDesc))

clearCmd :: IO ()
clearCmd = withState (`update` ClearCardDB)

cardCmd :: MultiverseID -> IO ()
cardCmd i = withState (\s -> query s (GetCard i) >>= myPrint)

nameCmd :: Name -> IO ()
nameCmd n = withState (\s -> query s (GetCardsByName n) >>= mapM_ myPrint)

namesCmd :: IO ()
namesCmd = withState (\s -> query s GetNames >>= mapM_ myPrint)

setsCmd :: IO ()
setsCmd = withState (\s -> query s GetCardSets >>= mapM_ myPrint)

persistCmd :: FilePath -> IO ()
persistCmd fp = withState (`persistCardSet` fp)

withState :: (AcidState CardDB -> IO r) -> IO r
withState f = do
    state <- openLocalState initialCardDB
    r <- f state
    closeAcidState state
    return r

persistCardSet :: AcidState CardDB -> FilePath -> IO ()
persistCardSet s fp = do
    Just cs' <- parseSet fp
    groupUpdates s $ map AddCard (persistableCards cs')
    update s (AddCardSet . persistableCardSet $ cs')

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

myColourPrefs :: HsColour.ColourPrefs
myColourPrefs = HsColour.defaultColourPrefs
  { HsColour.conid = [HsColour.Foreground HsColour.Magenta]
  , HsColour.conop = [HsColour.Foreground HsColour.Yellow]
  , HsColour.string = [HsColour.Foreground HsColour.Green]
  , HsColour.char = [HsColour.Foreground HsColour.Red]
  , HsColour.number = [HsColour.Foreground HsColour.Red]
  , HsColour.layout = [HsColour.Foreground HsColour.White]
  , HsColour.keyglyph = [HsColour.Foreground HsColour.White]
  }

myPrint :: Show a => a -> IO ()
myPrint = putStrLn . HsColour.hscolour
  (HsColour.TTYg HsColour.XTerm256Compatible) myColourPrefs
  False False "" False . IPPrint.pshow

----------------------------------------------------------

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

