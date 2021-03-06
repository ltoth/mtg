module Main where

import Control.Applicative
import Control.Lens hiding (argument)
import Control.Monad
import Data.Acid
import Data.Acid.Advanced
import Data.Either
import Data.List
import qualified Data.Text as T
import qualified IPPrint
import qualified Language.Haskell.HsColour as HsColour
import qualified Language.Haskell.HsColour.Colourise as HsColour
import qualified Language.Haskell.HsColour.Output as HsColour
import Options.Applicative

import Game.MtG.Acid
import Game.MtG.Types
import Game.MtG.CardTextParser (parseAndSetAbilities)
import Game.MtG.JSONParser (parseSet)
import Debug (engine)

opts :: Parser (IO ())
opts = subparser
   ( command "clear"   (info (pure clearCmd)
                       (progDesc "Clear the card database") )
  <> command "card"    (info (cardCmd <$>
                              argument auto (metavar "ID"))
                       (progDesc "Get parsed card by multiverseID") )
  <> command "name"    (info (nameCmd <$>
                              argument ((fmap.fmap) T.pack str) (metavar "NAME"))
                       (progDesc "Get parsed cards by exact name") )
  <> command "names"   (info (pure namesCmd)
                       (progDesc "Get all parsed card names") )
  <> command "sets"    (info (pure setsCmd)
                       (progDesc "Get all parsed sets") )
  <> command "persist" (info (persistCmd <$>
                              argument str (metavar "FILE"))
                       (progDesc "Parse set and cards and persist them") )
  <> command "debug"   (info (pure debugCmd)
                       (progDesc "Get all non-parsed cards") )
  <> command "engine"  (info (pure engineCmd)
                       (progDesc "Test game engine") )
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
persistCardSet s fp = parseSet fp >>= \pcs' ->
    case pcs' of
      Left  e   -> putStrLn e
      Right cs' -> do
        update s (AddCardSet . persistableCardSet $ cs')

        let cards = persistableCards cs'
        groupUpdates s $ map AddCard (rights cards)
        mapM_ putStrLn (lefts cards)

persistableCards :: CardSet' -> [Either String Card]
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

debugCmd :: IO ()
debugCmd = withState (\s -> do
    cs <- query s GetCards
    mapM_ myPrint . sort . map (view name) $
      filter (\c -> hasOtherEffect (c^.abilities)) cs
    )

engineCmd :: IO ()
engineCmd = void engine

abEffects :: Ability -> [Effect]
abEffects (ActivatedAbility _ es _) = es
abEffects (TriggeredAbility _ es _) = es
abEffects (SpellAbility es) = es
abEffects _ = []

hasEffect :: (Effect -> Bool) -> [Ability] -> Bool
hasEffect p as = any p (concatMap abEffects as)

hasOtherEffect :: [Ability] -> Bool
hasOtherEffect = hasEffect isOtherEffect

isETB :: Effect -> Bool
isETB (OtherEffect e) = "{This} enters the battlefield" `T.isInfixOf` e
isETB _ = False

hasTEOther :: [Ability] -> Bool
hasTEOther = any isTEOther
  where isTEOther (TriggeredAbility (TEOther _) _ _) = True
        isTEOther _ = False

