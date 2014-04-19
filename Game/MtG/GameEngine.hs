{-# LANGUAGE FlexibleContexts #-}

module Game.MtG.GameEngine where

import Control.Lens
import Control.Monad.Random.Class
import Control.Monad.State
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
-- import qualified Data.Text as T
import System.Random.Shuffle (shuffleM)

import Game.MtG.Types

testInitialGame :: Game
testInitialGame = initialGame
  [ ("Player 1", replicate 17 plains ++ replicate 23 yokedOx)
  , ("Player 2", replicate 17 forest ++ replicate 23 leafcrownDryad)
  ]

initialGame :: [(PlayerInfo, [Card])] -> Game
initialGame ps = execState createLibraries initGame
  where createLibraries = imapM_ createPlayerLib ps

        createPlayerLib i p =
          mapM (createObject i) (snd p) >>= assign (players.ix i.library)

        initGame = Game
          { _players = map (initPlayer . fst) ps
          , _battlefield = Set.empty
          , _stack = []
          , _exile = Set.empty
          , _commandZone = Set.empty
          , _activePlayerId = 0
          , _playerWithPriorityId = Nothing
          , _timestamp = 0
          , _turn = 0
          , _landCount = 0
          , _step = UntapStep
          , _relationships = initRelationships
          , _maxOId = 0
          }

        initPlayer pI = Player
          { _library = []
          , _hand = Set.empty
          , _graveyard = []
          , _life = 20
          , _poison = 0
          , _playerInfo = pI
          }

        initRelationships = Relationships
          { _attachedTo = IntMap.empty
          , _exiledWith = IntMap.empty
          }

createObject :: MonadState Game m => PId -> a -> m (Object a)
createObject p o = do
  i <- maxOId <+= 1
  return $ Object i p o

drawCard :: MonadState Game m => PId -> m ()
drawCard p = do
  mc <- preuse $ players.ix p.library.ix 0
  case mc of
    Just c  -> do
                 players.ix p.library %= drop 1
                 players.ix p.hand <>= Set.singleton c
    Nothing -> return () -- FIXME: p loses the game

shuffleLibrary :: (MonadState Game m, MonadRandom m) => PId -> m ()
shuffleLibrary p =
  (players.ix p.library) <~ (get >>= perform (players.ix p.library.act shuffleM))

---

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
