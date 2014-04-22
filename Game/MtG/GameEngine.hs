{-# LANGUAGE FlexibleContexts #-}

module Game.MtG.GameEngine where

import Control.Lens
import Control.Monad.Random.Class
import Control.Monad.State
import qualified Data.IntMap as IntMap
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
-- import qualified Data.Text as T
import System.Random.Shuffle (shuffleM)

import Game.MtG.Types

initialGame :: [(PlayerInfo, [Card])] -> Game
initialGame ps = execState createLibraries initGame
  where createLibraries = imapM_ createPlayerLib ps

        createPlayerLib i p =
          mapM (createObject i) (snd p) >>= assign (players.ix i.library)

        -- FIXME: Should be randomized, or set according to
        -- loser of last game etc.
        createTurnOrder = Seq.fromList $ imap const ps

        initGame = Game
          { _players = map (initPlayer . fst) ps
          , _battlefield = Set.empty
          , _stack = Seq.empty
          , _exile = Set.empty
          , _commandZone = Set.empty
          , _turnOrder = createTurnOrder
          , _activePlayer = 1  -- FIXME: Should be set to the last
                               -- player in turn order
          , _priority = Nothing
          , _successivePasses = Set.empty
          , _timestamp = 0
          , _turn = 0
          , _landCount = 0
          , _step = Cleanup
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

passPriority :: MonadState Game m => m ()
passPriority = do
  mp <- use priority
  case mp of
    Just p -> do
      sp <- successivePasses <<>= Set.singleton p
      ps <- use players
      if Set.size sp == length ps then do
        successivePasses .= Set.empty
        s <- use stack
        if Seq.null s then
          moveToNextStep
        else
          resolveTopOfStack
      else do
        np <- nextPlayerInTurnOrder p
        priority .= Just np
    Nothing -> return ()

moveToNextStep :: MonadState Game m => m ()
moveToNextStep = do
  priority .= Nothing
  ns <- step <%= succB
  case ns of
    UntapStep -> do
      turn += 1
      landCount .= 1
      aP <- use activePlayer
      np <- nextPlayerInTurnOrder aP
      activePlayer .= np
    _ -> return ()
  performTurnBasedActions ns
  performStateBasedActions
  aP <- use activePlayer
  priority .= Just aP   -- Even though no one should receive
                        -- priority during UntapStep and Cleanup,
                        -- the turn-based actions will immediately
                        -- moveToNextStep

performTurnBasedActions :: MonadState Game m => Step -> m ()
performTurnBasedActions UntapStep = do
  -- phaseInAndOut
  -- untapPermanents
  moveToNextStep
performTurnBasedActions DrawStep = do
  aP <- use activePlayer
  drawCard aP
performTurnBasedActions Cleanup = do
  -- discardToMaxHandSize
  -- removeMarkedDamage    -- This and the next one happen at once
  -- endUntilEndOfTurnEvents
  moveToNextStep
performTurnBasedActions _ = return ()

performStateBasedActions :: MonadState Game m => m ()
performStateBasedActions = return ()

succB :: (Bounded a, Enum a, Eq a) => a -> a
succB e | e == maxBound = minBound
        | otherwise     = succ e

resolveTopOfStack :: MonadState Game m => m ()
resolveTopOfStack = do
  -- TODO: Actually implement resolving
  aP <- use activePlayer
  priority .= Just aP


nextPlayerInTurnOrder :: MonadState Game m => PId -> m PId
nextPlayerInTurnOrder p = do
  tO <- use turnOrder
  case ifind (\_ v -> v==p) tO of
    Just (i, _) -> return $ tO^..cycled traverse^?!ix (succ i)
    Nothing     -> fail "Player not found in turn order"

shuffleLibrary :: (MonadState Game m, MonadRandom m) => PId -> m ()
shuffleLibrary p =
  (players.ix p.library) <~ (get >>= perform (players.ix p.library.act shuffleM))

