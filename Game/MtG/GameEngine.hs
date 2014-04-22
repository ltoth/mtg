{-# LANGUAGE FlexibleContexts #-}

module Game.MtG.GameEngine where

import Control.Lens
import Control.Monad.Random.Class
import Control.Monad.State
import Data.Foldable (Foldable)
import qualified Data.IntMap as IntMap
import Data.Maybe
import Data.Monoid
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
-- import qualified Data.Text as T
import Text.Read (readMaybe)
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
          , _maxTimestamp = 0
          , _turn = 0
          , _remainingLandCount = 0
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
          , _maxHandSize = 7
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

newTimestamp :: MonadState Game m => m Timestamp
newTimestamp = maxTimestamp <+= 1

cardToPermanent :: MonadState Game m => OCard -> m OPermanent
cardToPermanent oc = do
  t <- newTimestamp
  createObject (oc^.owner) PCard
    { _pcardCard = oc^.object
    , _pcardCharacteristics = cardToCharacteristics $ oc^.object
    , _pcardController = oc^.owner
    , _pcardPermanentStatus =
        PermanentStatus Untapped Unflipped FaceUp PhasedIn
    , _pcardSummoningSick = True
    , _pcardLoyaltyAlreadyActivated = False
    , _pcardTimestamp = t
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

playLand :: MonadState Game m => OId -> m ()
playLand i = do
  aP <- use activePlayer
  h <- use $ players.ix aP.hand
  case findOf folded (\o -> o^.oid == i) h of
    Just c  -> do
                 oP <- cardToPermanent c
                 players.ix aP.hand %= Set.delete c
                 battlefield <>= Set.singleton oP
                 remainingLandCount -= 1
    Nothing -> return ()

getAction :: (MonadState Game m, MonadIO m) => m ()
getAction = do
  la <- legalActions
  i <- chooseAction la  -- TODO: more general
  case la^?ix i of
    Just a  -> evalAction a
    Nothing -> getAction

chooseAction :: (MonadState Game m, MonadIO m) => Seq GameAction -> m Int
chooseAction as = do
  s <- get
  putIO . show $ s
  putIO . show $ as
  liftIO getLine >>= maybe (putIO "Invalid action" >> chooseAction as) return . readMaybe

putIO :: MonadIO m => String -> m ()
putIO = liftIO . putStrLn

evalAction :: MonadState Game m => GameAction -> m ()
evalAction (PlayLand i) = playLand i
evalAction PassPriority = passPriority
evalAction _ = return ()

legalActions :: MonadState Game m => m (Seq GameAction)
legalActions = do
  aP <- use activePlayer
  pr <- use priority
  s <- use stack
  st <- use step
  liftM mconcat . sequence $
    if Seq.null s &&
         pr == Just aP &&
         (st == PreCombatMain || st == PostCombatMain) then
       -- sorcery speed
      [ actionsPlayLands
      , actionsCastSorcerySpeed
      , actionsCastInstantSpeed
      , actionsPassPriority
      ]
    else if isJust pr then
      -- instant speed
      [ actionsCastInstantSpeed
      , actionsPassPriority ]
    else
      -- mana ability speed, or just nothing (if we implement
      -- mana abilities in the middle of casting spells and activating
      -- abilities)
      [ return Seq.empty ]
  where
    actionsPassPriority = return $ Seq.singleton PassPriority

    actionsPlayLands = do
      aP <- use activePlayer
      lc <- use remainingLandCount
      if lc > 0 then do
        h <- use $ players.ix aP.hand
        let landOIds = oidsMatchingPredicate
                       (\o -> Land `elem` o^.object.types) h
        return . Seq.fromList $ map PlayLand landOIds
      else return Seq.empty

    actionsCastSorcerySpeed = do
      aP <- use activePlayer
      h <- use $ players.ix aP.hand
      let sorceryOIds = oidsMatchingPredicate
                        (\o -> Land `notElem` o^.object.types) h
      return . Seq.fromList $ map CastSpell sorceryOIds

    actionsCastInstantSpeed = do
      Just p <- use priority  -- We can do this, because we checked
                              -- the timing in legalActions
      h <- use $ players.ix p.hand
      let instantOIds = oidsMatchingPredicate
                        (\o -> Instant `elem` o^.object.types ||
                         KeywordAbility Flash `elem` o^.object.abilities) h
      return . Seq.fromList $ map CastSpell instantOIds

oidsMatchingPredicate :: Foldable f => (OCard -> Bool) -> f OCard -> [OId]
oidsMatchingPredicate p f = f^..folded.filtered p^..traversed.oid

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
      remainingLandCount .= 1
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
  untapPermanents
  moveToNextStep
performTurnBasedActions DrawStep = do
  aP <- use activePlayer
  drawCard aP
performTurnBasedActions Cleanup = do
  -- discardToMaxHandSize
  removeMarkedDamage    -- This and the next one happen at once
  -- endUntilEndOfTurnEvents
  moveToNextStep
performTurnBasedActions _ = return ()

untapPermanents :: MonadState Game m => m ()
untapPermanents = return () -- TODO: Implement

removeMarkedDamage :: MonadState Game m => m ()
removeMarkedDamage = return () -- TODO: Implement

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

