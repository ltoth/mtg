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
import qualified IPPrint
import qualified Language.Haskell.HsColour as HsColour
import qualified Language.Haskell.HsColour.Colourise as HsColour
import qualified Language.Haskell.HsColour.Output as HsColour
import Text.Read (readMaybe)
import System.Random.Shuffle (shuffleM)

import Game.MtG.Types

-- |
-- = Top-level game engine
--
-- Runs in IO monad, as we need access to MonadRandom, and most
-- conceivable clients (except for AI) will need to do IO.

playGame :: App ()
playGame = do
  ps <- use players
  iforM_ ps $ \i _ -> do
    shuffleLibrary i
    replicateM_ 7 (drawCard i)
  -- resolveMulligans -- TODO: Implement
  moveToNextStep
  loopActions

loopActions :: App ()
loopActions = getAction >> loopActions

getAction :: (MonadState Game m, MonadIO m) => m ()
getAction = do
  la <- legalActions
  g <- get
  Just p <- use priority -- FIXME: What if there is no priority?

  -- TODO: This function should be looked up in player's state, so
  -- that we can support multiple UI clients and potentially AI
  i <- chooseAction (knownGame p g) la
  case la^?ix i of
    Just a  -> evalAction a
    Nothing -> getAction

initialGame :: [(PlayerInfo, [Card])] -> Game
initialGame ps = execState createLibraries initGame
  where createLibraries = imapM_ createPlayerLib ps

        createPlayerLib i p =
          mapM (createObject i) (snd p) >>= assign (players.ix i.library)

        -- FIXME: Should be randomized, or set according to
        -- loser of last game etc.
        createTurnOrder = Seq.fromList $ imap const ps

        initGame = Game
          { _gamePlayers = map (initPlayer . fst) ps
          , _gameBattlefield = Set.empty
          , _gameStack = Seq.empty
          , _gameExile = Set.empty
          , _gameCommandZone = Set.empty
          , _gameTurnOrder = createTurnOrder
          , _gameActivePlayer = 1  -- FIXME: Should be set to the last
                                   -- player in turn order
          , _gamePriority = Nothing
          , _gameSuccessivePasses = Set.empty
          , _gameMaxTimestamp = 0
          , _gameTurn = 0
          , _gameRemainingLandCount = 0
          , _gameStep = Cleanup
          , _gameRelationships = initRelationships
          , _gameMaxOId = 0
          }

        initPlayer pI = Player
          { _playerLibrary = []
          , _playerHand = Set.empty
          , _playerGraveyard = []
          , _playerLife = 20
          , _playerPoison = 0
          , _playerMaxHandSize = 7
          , _playerPlayerInfo = pI
          }

        initRelationships = Relationships
          { _attachedTo = IntMap.empty
          , _exiledWith = IntMap.empty
          }

-- |
-- = Determining legal game actions

legalActions :: MonadState Game m => m (Seq GameAction)
legalActions = do
  aP <- use activePlayer
  pr <- use priority
  s <- use stack
  st <- use step
  liftM (Seq.fromList . Set.toList . mconcat) . sequence $
    if Seq.null s &&
         pr == Just aP &&
         (st == PreCombatMain || st == PostCombatMain) then
       -- sorcery speed
      [ actionsPassPriority
      , actionsPlayLands
      , actionsCastSorcerySpeed
      , actionsCastInstantSpeed
      , actionsActivateAbilities
      ]
    else if isJust pr then
      -- instant speed
      [ actionsPassPriority
      , actionsCastInstantSpeed
      , actionsActivateAbilities
      ]
    else
      -- mana ability speed, or just nothing (if we implement
      -- mana abilities in the middle of casting spells and activating
      -- abilities)
      [ return Set.empty ]
  where
    actionsPassPriority = return $ Set.singleton PassPriority

    actionsPlayLands = do
      aP <- use activePlayer
      lc <- use remainingLandCount
      if lc > 0 then do
        h <- use $ players.ix aP.hand
        let landOIds = oidsMatchingPredicate
                       (\o -> Land `elem` o^.object.types) h
        return . Set.fromList $ map PlayLand landOIds
      else return Set.empty

    actionsCastSorcerySpeed = do
      aP <- use activePlayer
      h <- use $ players.ix aP.hand
      let sorceryOIds = oidsMatchingPredicate
                        (\o -> Land `notElem` o^.object.types) h
      return . Set.fromList $ map CastSpell sorceryOIds

    actionsCastInstantSpeed = do
      Just p <- use priority  -- We can do this, because we checked
                              -- the timing in legalActions
      h <- use $ players.ix p.hand
      let instantOIds = oidsMatchingPredicate
                        (\o -> Instant `elem` o^.object.types ||
                         KeywordAbility Flash `elem` o^.object.abilities) h
      return . Set.fromList $ map CastSpell instantOIds

    actionsActivateAbilities = do
      Just p <- use priority  -- We can do this, because we checked
                              -- the timing in legalActions
      b <- use battlefield
      let perms = b^..folded.filtered
                  (\o -> p == o^.object.controller).filtered
                  (\o -> anyOf each isActivatedAbility
                         (o^.object.characteristics.abilities))
      let aids = concatMap (\o ->
            zip (repeat $ o^.oid)
                (imap const
                  (o^.object.characteristics.abilities^..folded.filtered
                    isActivatedAbility))) perms
      return . Set.fromList $ map ActivateAbility aids

    oidsMatchingPredicate p f = f^..folded.filtered p^..traversed.oid

-- |
-- = Game actions chosen by players
--
-- Some of these might require a MonadIO constraint, as they
-- may require more choices (castSpell), while others are pure
-- (playLand, passPriority)

evalAction :: MonadState Game m => GameAction -> m ()
evalAction PassPriority  = passPriority
-- evalAction (CastSpell i) = castSpell i
evalAction (PlayLand i)  = playLand i
evalAction _ = return ()

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

-- |
-- = Internal game actions
--
-- Hopefully none of these require any player interaction, and
-- thus don't need to have a MonadIO constraint

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

resolveTopOfStack :: MonadState Game m => m ()
resolveTopOfStack = do
  -- TODO: Actually implement resolving
  aP <- use activePlayer
  priority .= Just aP

performTurnBasedActions :: MonadState Game m => Step -> m ()
performTurnBasedActions UntapStep = do
  -- phaseInAndOut
  untapPermanents
  -- resetSummoningSickness
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

-- |
-- = Helpers for internal game actions
--
-- None of these are complete game actions, they just help with
-- bookkeeping

-- | Turn the perfect information Game into a player's view of
-- the game, KGame
knownGame :: PId -> Game -> KGame
knownGame y g = KGame
  { _kgameYou = y
  , _kgamePlayers = knownPlayers (g^.players)
  , _kgameBattlefield = g^.battlefield
  , _kgameStack = g^.stack
  , _kgameExile = g^.exile
  , _kgameCommandZone = g^.commandZone
  , _kgameTurnOrder = g^.turnOrder
  , _kgameActivePlayer = g^.activePlayer
  , _kgamePriority = g^.priority
  , _kgameTurn = g^.turn
  , _kgameRemainingLandCount = g^.remainingLandCount
  , _kgameStep = g^.step
  , _kgameRelationships = g^.relationships  -- FIXME: What about hidden relationships?
  }
  where 
    knownPlayers = imap $ \i p ->
          if i == y then KPlayerYou
            { _kplayeryouLibrarySize = length (p^.library)
            , _kplayeryouHand = p^.hand
            , _kplayeryouGraveyard = p^.graveyard
            , _kplayeryouLife = p^.life
            , _kplayeryouPoison = p^.poison
            , _kplayeryouMaxHandSize = p^.maxHandSize
            , _kplayeryouPlayerInfo = p^.playerInfo
            }
          else KPlayerOpponent
            { _kplayeropponentLibrarySize = length (p^.library)
            , _kplayeropponentHandSize = Set.size (p^.hand)
            , _kplayeropponentGraveyard = p^.graveyard
            , _kplayeropponentLife = p^.life
            , _kplayeropponentPoison = p^.poison
            , _kplayeropponentMaxHandSize = p^.maxHandSize
            , _kplayeropponentPlayerInfo = p^.playerInfo
            }

nextPlayerInTurnOrder :: MonadState Game m => PId -> m PId
nextPlayerInTurnOrder p = do
  tO <- use turnOrder
  case ifind (\_ v -> v==p) tO of
    Just (i, _) -> return $ tO^..cycled traverse^?!ix (succ i)
    Nothing     -> fail "Player not found in turn order"

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

succB :: (Bounded a, Enum a, Eq a) => a -> a
succB e | e == maxBound = minBound
        | otherwise     = succ e

-- |
-- = UI client
--
-- TODO: Move this into Debug, or Main

chooseAction :: MonadIO m => KGame -> Seq GameAction -> m Int
chooseAction kg as = do
  liftIO . myPrint $ kg
  printActions as
  l <- liftIO getLine 
  maybe (putIO "Invalid action" >> chooseAction kg as) return (readMaybe l)
  where printActions = imapM_ (\i a -> putIO $ show i ++ ": " ++ show a)

putIO :: MonadIO m => String -> m ()
putIO = liftIO . putStrLn

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
