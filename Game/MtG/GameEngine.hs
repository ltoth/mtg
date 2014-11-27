{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Game.MtG.GameEngine where

import Control.Lens
import Control.Monad
import Control.Monad.Random.Class
import Control.Monad.Loops
import Control.Monad.State
import Data.Data.Lens (biplate)
import Data.Foldable (toList)
import qualified Data.IntMap as IntMap
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random.Shuffle (shuffleM)

import Game.MtG.Types
import Game.MtG.Client.Console

-- |
-- = Top-level game engine
--
-- Runs in IO monad, as we need access to MonadRandom, and most
-- conceivable clients (except for AI) will need to do IO.
--
-- All rules cross-reference MagicCompRules_20140201.pdf

playGame :: App ()
playGame = do
  ps <- use players
  iforM_ ps $ \i _ -> do
    shuffleLibrary i
    replicateM_ 7 (drawCard i)
  -- resolveMulligans -- TODO: Implement
  moveToNextStep
  loopPriorityActions

getPlayerChoice
  :: (MonadState Game m, MonadIO m)
  => PId
  -> SPlayerChoice c
  -> PlayerChoiceRequest c
  -> m (PlayerChoiceResponse c)
getPlayerChoice pid pc req = do
  mp <- preuse $ players.ix pid
  case mp of
    Nothing -> fail "Player does not exist"
    Just p -> do
      g <- get

      -- TODO: update all other players with their knownGame

      let kg = knownGame pid g
      resp <- getValidChoice kg
      choiceLog %= (|> PlayerChoiceLog pid pc resp)
      return resp
      where
        getValidChoice kg = do
          resp <- choiceFn p pc kg req
          case pc of
            SChoosePriorityAction ->
              if resp `Set.member` req
                then return resp
                else getValidChoice kg
            SChooseModes -> return resp
              -- TODO: implement
            SChooseManaAbilityActivation -> return resp
              -- TODO: implement

loopPriorityActions :: App ()
loopPriorityActions = getPriorityAction >> loopPriorityActions

-- Ask the player with priority which action he'd like to take
getPriorityAction :: (MonadState Game m, MonadIO m) => m ()
getPriorityAction = do
  mp <- use priority
  case mp of
    Just p  -> do
      la <- legalActions p
      a <- getPlayerChoice p SChoosePriorityAction la
      evalAction a p

    Nothing ->
      -- This means the game actions left the game inconsistent
      fail "No player had priority when we tried to get legalActions"

initialGame :: [(PlayerInfo, [Card])] -> Game
initialGame ps = execState createLibraries initGame
  where createLibraries = imapM_ createPlayerLib ps

        createPlayerLib i (_, cs) =
          forM cs $ (\c -> do
            ci <- newOId
            players.ix i.library %= ((ci, c) <|)) . OCard i

        -- FIXME: Should be randomized, or set according to
        -- loser of last game etc.
        createTurnOrder = Seq.fromList $ imap const ps

        initGame = Game
          { _gamePlayers = map (initPlayer . fst) ps
          , _gameBattlefield = IntMap.empty
          , _gameStack = Seq.empty
          , _gameExile = IntMap.empty
          , _gameCommandZone = IntMap.empty
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
          , _gameChoiceLog = Seq.empty
          }

        initPlayer pI = Player
          { choiceFn = consoleChoiceFn
          , _playerLibrary = Seq.empty
          , _playerHand = IntMap.empty
          , _playerGraveyard = Seq.empty
          , _playerLife = 20
          , _playerPoison = 0
          , _playerMaxHandSize = 7
          , _playerManaPool = initManaPool
          , _playerPlayerInfo = pI
          }

        initRelationships = Relationships
          { _attachedTo = IntMap.empty
          , _exiledWith = IntMap.empty
          }

        initManaPool = ManaPool
          { _whiteMana = 0
          , _blueMana = 0
          , _blackMana = 0
          , _redMana = 0
          , _greenMana = 0
          , _colorlessMana = 0
          }

-- |
-- = Determining legal game actions

-- | Get the set of legal actions for the passed in player with
-- priority. Return a Seq rather than Set so that it can be indexed.
legalActions :: MonadState Game m => PId -> m (Set PriorityAction)
legalActions pr = do
  aP <- use activePlayer
  s <- use stack
  st <- use step
  liftM mconcat . mapM ($ pr) $
    -- instant speed
    [ actionsPassPriority
    , actionsCastInstantSpeed
    , actionsActivateAbilities
    , actionsManaAbilities
    ] ++
    -- sorcery speed
    if Seq.null s && pr == aP &&
         (st == PreCombatMain || st == PostCombatMain) then
      [ actionsPlayLands
      , actionsCastSorcerySpeed
      , actionsLoyaltyAbilities
      ]
    else []

actionsPassPriority :: MonadState Game m => PId -> m (Set PriorityAction)
actionsPassPriority _ = return $ Set.singleton PassPriority

actionsPlayLands :: MonadState Game m => PId -> m (Set PriorityAction)
actionsPlayLands p = do
  lc <- use remainingLandCount
  if lc > 0 then do
    h <- use $ players.ix p.hand
    let os = map fst $ h^@..ifolded.filtered
             (\o -> Land `elem` o^.card.types)
    return . Set.fromList $ map PlayLand os
  else return Set.empty

actionsCastSorcerySpeed :: MonadState Game m => PId -> m (Set PriorityAction)
actionsCastSorcerySpeed p = do
  h <- use $ players.ix p.hand
  let os = map fst $ h^@..ifolded.filtered
           (\o -> Land `notElem` o^.card.types)
  return . Set.fromList $ map CastSpell os

actionsCastInstantSpeed :: MonadState Game m => PId -> m (Set PriorityAction)
actionsCastInstantSpeed p = do
  h <- use $ players.ix p.hand
  let os = map fst $ h^@..ifolded.filtered
           (\o -> Instant `elem` o^.card.types ||
                  KeywordAbility Flash `elem` o^.card.abilities)
  return . Set.fromList $ map CastSpell os

actionsActivateAbilities :: MonadState Game m => PId -> m (Set PriorityAction)
actionsActivateAbilities p = do
  b <- use battlefield
  let as = aids isRegularActivatedAbility $
             b^@..ifolded.filtered (controlledBy p).
             filtered (hasLegalAbilities isRegularActivatedAbility)
  return . Set.fromList $ map ActivateAbility as

actionsManaAbilities :: MonadState Game m => PId -> m (Set PriorityAction)
actionsManaAbilities p = do
  b <- use battlefield
  let as = aids isManaAbility $
             b^@..ifolded.filtered (controlledBy p).
             filtered (hasLegalAbilities isManaAbility)
  return . Set.fromList $ map ActivateManaAbility as

actionsLoyaltyAbilities :: MonadState Game m => PId -> m (Set PriorityAction)
actionsLoyaltyAbilities p = do
  b <- use battlefield
  let as = aids isLoyaltyAbility $
             b^@..ifolded.filtered (controlledBy p).
             filtered (hasLegalAbilities isLoyaltyAbility).
             filtered loyaltyNotYetActivated
  return . Set.fromList $ map ActivateLoyaltyAbility as

aids :: (Ability -> Bool) -> [(OId, Permanent)] -> [AId]
aids f = concatMap (\(i,o) ->
         zip (repeat i)
             (imap const
                   (o^.chars.abilities^..folded.filtered f)))

controlledBy :: PId -> Permanent -> Bool
controlledBy p o = o^.controller == p

loyaltyNotYetActivated :: Permanent -> Bool
loyaltyNotYetActivated o = not $ o^.loyaltyAlreadyActivated

hasLegalAbilities :: (Ability -> Bool) -> Permanent -> Bool
hasLegalAbilities f o = anyOf each (\a -> f a && ifRequiresTapCanBe a)
                     (o^.chars.abilities)
  where ifRequiresTapCanBe a =
          -- TODO: restrict the biplate to (a^.activationCost)
          not (anyOf biplate isCTap a)
            || (o^.permanentStatus.tapStatus == Untapped)

isRegularActivatedAbility :: Ability -> Bool
isRegularActivatedAbility a = isActivatedAbility a
                              && (not . isManaAbility $ a)
                              && (not . isLoyaltyAbility $ a)

-- rule 605.1a
isManaAbility :: Ability -> Bool
isManaAbility a = isActivatedAbility a
                  && (not . anyOf biplate isTarget $ a)
                  && anyOf biplate isAddMana a
                  && (not . isLoyaltyAbility $ a)

isLoyaltyAbility :: Ability -> Bool
isLoyaltyAbility (ActivatedAbility [CLoyalty _] _ _) = True
isLoyaltyAbility _ = False

-- |
-- = Game actions chosen by players
--
-- Some of these might require a MonadIO constraint, as they
-- may require more choices (castSpell), while others are pure
-- (playLand, passPriority)

evalAction :: (MonadState Game m, MonadIO m) => PriorityAction -> PId -> m ()
evalAction PassPriority               = passPriority
evalAction (CastSpell o)              = castSpell o
evalAction (ActivateAbility a)        = activateAbility a
evalAction (ActivateManaAbility a)    = activateManaAbility a
evalAction (ActivateLoyaltyAbility a) = activateLoyaltyAbility a
evalAction (PlayLand o)               = playLand o

castSpell :: (MonadState Game m, MonadIO m) => OId -> PId -> m ()
castSpell i p = do
  h <- use $ players.ix p.hand
  case h^.at i of
    Nothing -> return ()
    Just c -> do
      g1 <- get  -- save the state in case we need to rewind

      -- rule 116.4 (casting a spell resets the set of players)
      successivePasses .= Set.empty

      -- rule 601.2a (card moves from hand to stack)
      players.ix p.hand %= sans i
      let oS = cardToSpell c
      oi <- newOId
      stack %= ((oi, oS) <|)

      -- rule 601.2b (modes, splice, alternative, additional,
      -- variable cost, hybrid mana, Phyrexian mana)
      -- TODO: implement asking for these choices where appropriate

      -- rule 601.2c (choose targets)
      -- TODO: implement

      -- rule 601.2d (choose division)
      -- TODO: implement

      -- rule 601.2e (calculate total cost)
      -- TODO: make this more general
      let mc = fromMaybe [] $ oS^._OSpell.chars.manaCost
          tc = map resolveCost [CMana mc]

      -- rule 601.2f (activate mana abilities)
      activateAnyManaAbilities

      -- rule 601.2g (pay total cost)
      paid <- mapM (payCost p oi) tc

      -- rule 601.2h (trigger spell cast, put onto stack)

      -- rule 116.3c (same player keeps priority)
      unless (andOf each paid) $ put g1 -- rewind

  where
    -- totalCost :: Maybe ManaCost -> [Cost] ->
    activateAnyManaAbilities = do
      mas <- actionsManaAbilities p
      unless (Set.null mas) $ do
        ma <- getPlayerChoice p SChooseManaAbilityActivation mas
        case ma of
          Just a  -> evalAction a p >> activateAnyManaAbilities
          Nothing -> return ()

-- TODO: implement
activateAbility :: (MonadState Game m, MonadIO m) => AId -> PId -> m ()
activateAbility a p = return ()

activateManaAbility :: (MonadState Game m, MonadIO m) => AId -> PId -> m ()
activateManaAbility (oi, ai) p = do
  b <- use battlefield
  case b^?ifolded.filtered (controlledBy p).index oi of
    Nothing -> return ()
    Just o  ->
      case o^?chars.abilities.ix ai of
        Nothing -> return ()
        Just (ActivatedAbility cs es ainst) -> do
          -- save the state in case we have to rewind
          g1 <- get

          -- TODO: implement all the steps of activating abilities

          let rcs = map resolveCost cs

          paid <- mapM (payCost p oi) rcs
          if andOf each paid
            then mapM_ (resolveEffect p oi) es
            else put g1  -- rewind
        Just _ -> return ()
                  -- TODO: this should never happen; it should always be an
                  -- activated ability. what to do?

resolveCost :: Cost -> ResolvedCost
resolveCost (CMana mc)   = CMana' $ resolveManaCost mc
resolveCost CTap         = CTap'
resolveCost CUntap       = CUntap'
resolveCost (CLoyalty n) = CLoyalty' n
resolveCost (CEffect e)  = CEffect' e

resolveManaCost :: ManaCost -> ResolvedManaCost
resolveManaCost = concatMap go
  where go W = [W']
        go U = [U']
        go B = [B']
        go R = [R']
        go G = [G']
        go (CL n) = replicate (fromIntegral n) CL'
        go _ = []  -- TODO: How to resolve other mana costs?

-- Needs MonadIO as it may need to ask how the costs should be paid
-- OId is the context (what it TMThis, what should be CTap'ed)
-- Returns the success of paying the cost
payCost :: (MonadState Game m, MonadIO m) => PId -> OId -> ResolvedCost -> m Bool
payCost p _ (CMana' rmc) = do
  let (colorless, colored) = partition isCL' rmc
  coloredPaid <- mapM payOne colored
  -- all colored must be paid with the correct mana
  if andOf each coloredPaid
    then do
      -- first, try paying colorless with colorless
      clPaid <- mapM payOne colorless
      let remaining = length $ filter not clPaid
      if remaining == 0
        -- everything's been paid
        then return True
        else do
          mp <- preuse $ players.ix p.manaPool
          let avail = sumOf biplate mp :: Int
          case remaining `compare` avail of
            GT -> return False
            EQ -> do
              mapM_ (iterateWhile id . payOne) [W' .. G']
              return True
            LT ->
              -- TODO: is all the available mana the same color?
              -- if so, just pay it
              -- TODO: otherwise, ask which colors should be used
              return False
    else return False
  where
    payOne rms = do
      Just mp <- preuse $ players.ix p.manaPool.(cloneLens . rmsLens $ rms)
      if mp > 0 then do
        players.ix p.manaPool.(cloneLens . rmsLens $ rms) -= 1
        return True
      else return False

payCost _ i CTap' = do
  b <- use battlefield
  case b^.at i of
    Nothing -> return False
    Just o ->
      case o^.permanentStatus.tapStatus of
        Tapped   -> return False
        Untapped -> do
          battlefield.ix i.permanentStatus.tapStatus .= Tapped
          return True

payCost _ _ _ = return False


-- TODO: should also take Maybe StackObject for targets, X, etc.
resolveEffect :: (MonadState Game m, MonadIO m) => PId -> OId -> Effect -> m ()
resolveEffect p i (AddMana Nothing (ManaSymbols [ms])) =
  mapM_ (addManaSymbolToPool p) (resolveManaCost ms)

resolveEffect _ _ _ = return ()

-- TODO: implement
activateLoyaltyAbility :: (MonadState Game m, MonadIO m) => AId -> PId -> m ()
activateLoyaltyAbility a p = return ()

playLand :: MonadState Game m => OId -> PId -> m ()
playLand i p = do
  h <- use $ players.ix p.hand
  case h^.at i of
    Just c -> do
      players.ix p.hand %= sans i
      o <- cardToPermanent c
      oi <- newOId
      battlefield.at oi ?= o
      remainingLandCount -= 1
    Nothing -> return ()

passPriority :: MonadState Game m => PId -> m ()
passPriority p = do
  sp <- successivePasses <<>= Set.singleton p
  ps <- use players

  -- rule 116.4 (all players passed in succession)
  if Set.size sp == length ps then do
    successivePasses .= Set.empty
    s <- use stack
    if Seq.null s then
      moveToNextStep
    else
      -- rule 608.1
      resolveTopOfStack
  -- rule 116.3d (next player receives priority)
  else do
    np <- nextPlayerInTurnOrder p
    givePlayerPriority np

-- |
-- = Internal game actions
--
-- Hopefully none of these require any player interaction, and
-- thus don't need to have a MonadIO constraint

resolveTopOfStack :: MonadState Game m => m ()
resolveTopOfStack = do
  -- TODO: Actually implement resolving
  -- rule 608.2
  moS <- preuse $ stack.ix 0
  stack %= Seq.drop 1
  case moS of
    Nothing -> return ()  -- should never happen: stack shouldn't be
                          -- empty when this is called
    Just (oi, OSpell s) -> do
      let ts = s^.chars.types
      if Instant `elem` ts || Sorcery `elem` ts then do
        -- TODO: implement, mapM_ resolveEffect
        return ()
        return ()
      else do
        -- permanent
        o <- spellToPermanent s
        noi <- newOId
        battlefield.at noi ?= o
    Just (oi, OStackAbility sa) -> do
      -- TODO: implement, mapM_ resolveEffect
      return ()
      return ()

  -- rule 116.3b
  aP <- use activePlayer
  givePlayerPriority aP

addManaSymbolToPool :: MonadState Game m => PId -> ResolvedManaSymbol -> m ()
addManaSymbolToPool p rms =
    players.ix p.manaPool.(cloneLens . rmsLens $ rms) += 1

emptyManaPools :: MonadState Game m => m ()
emptyManaPools = players.each.manaPool.each .= 0

moveToNextStep :: MonadState Game m => m ()
moveToNextStep = do
  priority .= Nothing
  emptyManaPools
  ns <- step <%= succB
  case ns of
    -- new turn
    UntapStep -> do
      turn += 1
      remainingLandCount .= 1
      aP <- use activePlayer
      np <- nextPlayerInTurnOrder aP
      activePlayer .= np
    _ -> return ()
  performTurnBasedActions ns

  -- rule 116.3a
  aP <- use activePlayer
  givePlayerPriority aP -- Even though no one should receive
                        -- priority during UntapStep and Cleanup,
                        -- the turn-based actions will immediately
                        -- moveToNextStep

givePlayerPriority :: MonadState Game m => PId -> m ()
givePlayerPriority p = do
  -- rule 116.5 (each time a player would get priority...)
  performStateBasedActions
  -- putTriggeredAbilitiesOnStack
  priority .= Just p

performTurnBasedActions :: MonadState Game m => Step -> m ()
performTurnBasedActions UntapStep = do
  phaseInAndOut
  untapPermanents
  resetSummoningSickness
  moveToNextStep
performTurnBasedActions DrawStep = do
  aP <- use activePlayer
  t <- use turn
  -- TODO: Generalize to multiplayer games with a setting in
  -- Game (and argument in initialGame)
  unless (t == 1) (drawCard aP)
performTurnBasedActions Cleanup = do
  -- discardToMaxHandSize
  removeMarkedDamage    -- This and the next one happen at once
  -- endUntilEndOfTurnEvents

  -- TODO: rule 514.3a (if stateBasedActions or triggeredAbilities
  -- would happen, perform them, then aP gets priority; once
  -- players pass in succession, there is another Cleanup)

  moveToNextStep
performTurnBasedActions _ = return ()

phaseInAndOut :: MonadState Game m => m ()
phaseInAndOut = do
  aP <- use activePlayer
  -- TODO: First phase out indirectly (need attachedTo relationship)
  -- FIXME: Invalid traversal?
  battlefield.traversed.filtered (controlledBy aP).filtered
    (\o -> KeywordAbility Phasing `elem` o^.chars.abilities
           && o^.permanentStatus.phaseStatus == PhasedIn).
    permanentStatus.phaseStatus .= PhasedOut
  -- TODO: First phase in indirectly (need attachedTo relationship)
  battlefield.traversed.filtered (controlledBy aP).filtered
    (\o -> o^.permanentStatus.phaseStatus == PhasedOut).
    permanentStatus.phaseStatus .= PhasedIn

untapPermanents :: MonadState Game m => m ()
untapPermanents = do
  aP <- use activePlayer
  battlefield.traversed.filtered (controlledBy aP).
    permanentStatus.tapStatus .= Untapped

resetSummoningSickness :: MonadState Game m => m ()
resetSummoningSickness = do
  aP <- use activePlayer
  battlefield.traversed.filtered (controlledBy aP).
    summoningSick .= False

removeMarkedDamage :: MonadState Game m => m ()
removeMarkedDamage = battlefield.traversed.markedDamage .= 0

performStateBasedActions :: MonadState Game m => m ()
performStateBasedActions =
    -- TODO: rule 704.3: if any SBAs are performed, repeat the check
    return ()

drawCard :: MonadState Game m => PId -> m ()
drawCard p = do
  mc <- preuse $ players.ix p.library.ix 0
  case mc of
    Just (ci, c) -> do
                 players.ix p.library %= Seq.drop 1
                 players.ix p.hand.at ci ?= c
    Nothing -> return () -- FIXME: p loses the game

mulliganHand :: (MonadState Game m, MonadRandom m) => PId -> m ()
mulliganHand p = do
  h <- use $ players.ix p.hand
  iforM_ h $ \i _ -> putCardFromHandOnTopOfLibrary p i
  shuffleLibrary p
  replicateM_ (IntMap.size h - 1) (drawCard p)

putCardFromHandOnTopOfLibrary :: MonadState Game m => PId -> OId -> m ()
putCardFromHandOnTopOfLibrary p i = do
  h <- use $ players.ix p.hand
  case h^.at i of
    Just c -> do
      players.ix p.hand %= sans i
      players.ix p.library %= ((i, c) <|)
    Nothing -> return ()

shuffleLibrary :: (MonadState Game m, MonadRandom m) => PId -> m ()
shuffleLibrary p =
  -- TODO: Make work with Seq (OId, OCard)
  (players.ix p.library) <~ (get >>= perform (players.ix p.library.act seqShuffleM))
  where seqShuffleM as = do
          ss <- shuffleM . toList $ as
          return $ Seq.fromList ss

-- |
-- = Helpers for internal game actions
--
-- None of these are complete game actions, they just help with
-- bookkeeping

-- | Turn the perfect information Game into a player's view of
-- the game, KGame
knownGame :: PId -> Game -> KGame
knownGame y g = KGame
  { _kgameChoiceLog = g^.choiceLog
  , _kgameYou = y
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
            { _kplayeryouLibrarySize = Seq.length (p^.library)
            , _kplayeryouHand = p^.hand
            , _kplayeryouGraveyard = p^.graveyard
            , _kplayeryouLife = p^.life
            , _kplayeryouPoison = p^.poison
            , _kplayeryouMaxHandSize = p^.maxHandSize
            , _kplayeryouManaPool = p^.manaPool
            , _kplayeryouPlayerInfo = p^.playerInfo
            }
          else KPlayerOpponent
            { _kplayeropponentLibrarySize = Seq.length (p^.library)
            , _kplayeropponentHandSize = IntMap.size (p^.hand)
            , _kplayeropponentGraveyard = p^.graveyard
            , _kplayeropponentLife = p^.life
            , _kplayeropponentPoison = p^.poison
            , _kplayeropponentManaPool = p^.manaPool
            , _kplayeropponentMaxHandSize = p^.maxHandSize
            , _kplayeropponentPlayerInfo = p^.playerInfo
            }

nextPlayerInTurnOrder :: MonadState Game m => PId -> m PId
nextPlayerInTurnOrder p = do
  tO <- use turnOrder
  case ifind (\_ v -> v==p) tO of
    Just (i, _) -> return $ tO^..cycled traverse^?!ix (succ i)
    Nothing     -> fail "Player not found in turn order"

newOId :: MonadState Game m => m OId
newOId = maxOId <+= 1

newTimestamp :: MonadState Game m => m Timestamp
newTimestamp = maxTimestamp <+= 1

cardToPermanent :: MonadState Game m => OCard -> m Permanent
cardToPermanent oc = do
  t <- newTimestamp
  return PCard
    { _pcardCard = oc^.card
    , _pcardChars = cardToCharacteristics $ oc^.card
    , _pcardOwner = oc^.owner
    , _pcardController = oc^.owner
    , _pcardPermanentStatus =
        PermanentStatus Untapped Unflipped FaceUp PhasedIn
    , _pcardSummoningSick = True
    , _pcardMarkedDamage = 0
    , _pcardLoyaltyAlreadyActivated = False
    , _pcardTimestamp = t
    }

spellToPermanent :: MonadState Game m => Spell -> m Permanent
spellToPermanent s = do
  t <- newTimestamp
  return PCard
    { _pcardCard = s^.card
    , _pcardChars = cardToCharacteristics $ s^.card
    , _pcardOwner = s^.owner
    , _pcardController = s^.owner
    , _pcardPermanentStatus =
        PermanentStatus Untapped Unflipped FaceUp PhasedIn
    , _pcardSummoningSick = True
    , _pcardMarkedDamage = 0
    , _pcardLoyaltyAlreadyActivated = False
    , _pcardTimestamp = t
    }

cardToSpell :: OCard -> StackObject
cardToSpell oc = OSpell Spell
    { _spellCard = oc^.card
    , _spellChars = cardToCharacteristics $ oc^.card
    , _spellOwner = oc^.owner
    , _spellController = oc^.owner
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

rmsLens :: ResolvedManaSymbol -> Lens' ManaPool Int
rmsLens W'  = whiteMana
rmsLens U'  = blueMana
rmsLens B'  = blackMana
rmsLens R'  = redMana
rmsLens G'  = greenMana
rmsLens CL' = colorlessMana

succB :: (Bounded a, Enum a, Eq a) => a -> a
succB e | e == maxBound = minBound
        | otherwise     = succ e
