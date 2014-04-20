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

cardToPermanent :: MonadState Game m => OCard -> m OPermanent
cardToPermanent oc = createObject (oc^.owner) PCard
  { _pcardCard = oc^.object
  , _pcardCharacteristics = cardToCharacteristics $ oc^.object
  , _pcardController = oc^.owner
  , _pcardPermanentStatus =
      PermanentStatus Untapped Unflipped FaceUp PhasedIn
  , _pcardSummoningSick = True
  , _pcardLoyaltyAlreadyActivated = False
  }

cardToCharacteristics :: Card -> Characteristics
cardToCharacteristics c = Characteristics
  { _characteristicsName = c^.name
  , _characteristicsManaCost = c^.manaCost
  , _characteristicsColors = c^.colors
  , _characteristicsTypes = c^.types
  , _characteristicsSubtypes = c^.subtypes
  , _characteristicsSupertypes = c^.supertypes
  , _characteristicsRulesText = c^.rulesText
  , _characteristicsAbilities = c^.abilities
  , _characteristicsPower = c^.power
  , _characteristicsToughness = c^.toughness
  , _characteristicsLoyalty = c^.loyalty
  }

drawCard :: MonadState Game m => PId -> m ()
drawCard p = do
  mc <- preuse $ players.ix p.library.ix 0
  case mc of
    Just c  -> do
                 players.ix p.library %= drop 1
                 players.ix p.hand <>= Set.singleton c
    Nothing -> return () -- FIXME: p loses the game

playLand :: MonadState Game m => PId -> OId -> m ()
playLand p i = do
  h <- use $ players.ix p.hand
  case findOf folded (\o -> o^.oid == i) h of
    Just c  -> do
                 oP <- cardToPermanent c
                 players.ix p.hand %= Set.delete c
                 battlefield <>= Set.singleton oP
    Nothing -> return ()

shuffleLibrary :: (MonadState Game m, MonadRandom m) => PId -> m ()
shuffleLibrary p =
  (players.ix p.library) <~ (get >>= perform (players.ix p.library.act shuffleM))

