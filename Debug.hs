module Debug where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Data.Lens (biplate)
import Data.Either
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Game.MtG.GameEngine
import Game.MtG.Types
import Game.MtG.CardTextParser (parseAndSetAbilities)
import Game.MtG.JSONParser (parseSet)

main :: IO Game
main = execStateT playGame testInitialGame

playGame :: StateT Game IO ()
playGame = do
  ps <- use players
  iforM_ ps $ \i _ -> do
    shuffleLibrary i
    replicateM_ 7 (drawCard i)
  moveToNextStep

testInitialGame :: Game
testInitialGame = initialGame
  [ ("Player 1", replicate 17 plains ++ replicate 23 yokedOx)
  , ("Player 2", replicate 17 forest ++ replicate 23 leafcrownDryad)
  ]

yokedOx :: Card
yokedOx = Card{_cardLayout = Normal, _cardTypeLine = "Creature \8212 Ox",
         _cardTypes = [Creature], _cardColors = [White],
         _cardMultiverseID = 373572, _cardName = "Yoked Ox", _cardNames = [],
         _cardSupertypes = [], _cardSubtypes = [CreatureType Ox],
         _cardCmc = Just 1, _cardRarity = Common, _cardArtist = "Ryan Yee",
         _cardPower = Just "0", _cardToughness = Just "4",
         _cardLoyalty = Nothing, _cardManaCost = Just [W],
         _cardRulesText = Nothing, _cardAbilities = [], _cardCardNumber = "37",
         _cardVariations = [], _cardImageName = "yoked ox",
         _cardWatermark = Nothing, _cardCardBorder = Nothing,
         _cardSetCode = "THS"}

plains :: Card
plains = Card{_cardLayout = Normal, _cardTypeLine = "Basic Land \8212 Plains",
         _cardTypes = [Land], _cardColors = [], _cardMultiverseID = 373654,
         _cardName = "Plains", _cardNames = [], _cardSupertypes = [Basic],
         _cardSubtypes = [LandType (BasicLand Plains)], _cardCmc = Nothing,
         _cardRarity = BasicLandRarity, _cardArtist = "Rob Alexander",
         _cardPower = Nothing, _cardToughness = Nothing, _cardLoyalty = Nothing,
         _cardManaCost = Nothing, _cardRulesText = Just "W",
         _cardAbilities =
           [ActivatedAbility [CTap] [AddMana Nothing (ManaSymbols [[W]])]
              Nothing],
         _cardCardNumber = "230", _cardVariations = [373533, 373582, 373700],
         _cardImageName = "plains1", _cardWatermark = Nothing,
         _cardCardBorder = Nothing, _cardSetCode = "THS"}

forest :: Card
forest = Card{_cardLayout = Normal, _cardTypeLine = "Basic Land \8212 Forest",
         _cardTypes = [Land], _cardColors = [], _cardMultiverseID = 373625,
         _cardName = "Forest", _cardNames = [], _cardSupertypes = [Basic],
         _cardSubtypes = [LandType (BasicLand Forest)], _cardCmc = Nothing,
         _cardRarity = BasicLandRarity, _cardArtist = "Steven Belledin",
         _cardPower = Nothing, _cardToughness = Nothing, _cardLoyalty = Nothing,
         _cardManaCost = Nothing, _cardRulesText = Just "G",
         _cardAbilities =
           [ActivatedAbility [CTap] [AddMana Nothing (ManaSymbols [[G]])]
              Nothing],
         _cardCardNumber = "247", _cardVariations = [373568, 373615, 373688],
         _cardImageName = "forest2", _cardWatermark = Nothing,
         _cardCardBorder = Nothing, _cardSetCode = "THS"}

leafcrownDryad :: Card
leafcrownDryad = Card{_cardLayout = Normal,
         _cardTypeLine = "Enchantment Creature \8212 Nymph Dryad",
         _cardTypes = [Enchantment, Creature], _cardColors = [Green],
         _cardMultiverseID = 373523, _cardName = "Leafcrown Dryad",
         _cardNames = [], _cardSupertypes = [],
         _cardSubtypes = [CreatureType Nymph, CreatureType Dryad],
         _cardCmc = Just 2, _cardRarity = Common, _cardArtist = "Volkan Baga",
         _cardPower = Just "2", _cardToughness = Just "2",
         _cardLoyalty = Nothing, _cardManaCost = Just [CL 1, G],
         _cardRulesText =
           Just
             "Bestow {3}{G} (If you cast this card for its bestow cost, it's an Aura spell with enchant creature. It becomes a creature again if it's not attached to a creature.)\n\nReach\n\nEnchanted creature gets +2/+2 and has reach.",
         _cardAbilities =
           [KeywordAbility (Bestow [CMana [CL 3, G]]), KeywordAbility Reach,
            SpellAbility
              [ModifyPT (NoTarget Nothing [TMEnchantedPermanent])
                 (Plus (NumValue 2))
                 (Plus (NumValue 2))
                 Nothing,
               AddAbilities (NoTarget Nothing [TMIt]) [KeywordAbility Reach]
                 Nothing]],
         _cardCardNumber = "161", _cardVariations = [],
         _cardImageName = "leafcrown dryad", _cardWatermark = Nothing,
         _cardCardBorder = Nothing, _cardSetCode = "THS"}

---

setFile :: String
setFile = "THS.json"

debugGetCards :: FilePath -> IO (Either String [Card])
debugGetCards fp = (view cards' <$>) <$> parseSet fp

filterCards :: (Card -> Bool) -> IO [Card]
filterCards p = go <$> debugGetCards setFile
  where go = rights .
             fmap parseAndSetAbilities .
             filter p .
             either (const []) id

cardTextIncludes :: Text -> Card -> Bool
cardTextIncludes s = fromMaybe False .
                     (view rulesText >=> return . T.isInfixOf s)

nameStartsWith :: Text -> Card -> Bool
nameStartsWith s = T.isPrefixOf s . view name

---

-- map doesTarget <$> filterCards (nameStartsWith "Rescue")

doesTarget :: Card -> Bool
doesTarget c = anyOf biplate isTarget (c^.abilities)

isTarget :: Targets -> Bool
isTarget (Target _ _) = True
isTarget _            = False
