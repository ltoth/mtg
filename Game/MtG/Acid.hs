{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Game.MtG.Acid where

import Control.Applicative
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.Data
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.SafeCopy

import Game.MtG.Types

$(deriveSafeCopy 0 'base ''Layout)
$(deriveSafeCopy 0 'base ''ManaSymbol)
$(deriveSafeCopy 0 'base ''ManaType)
$(deriveSafeCopy 0 'base ''Color)
$(deriveSafeCopy 0 'base ''Supertype)
$(deriveSafeCopy 0 'base ''Type)
$(deriveSafeCopy 0 'base ''Subtype)
$(deriveSafeCopy 0 'base ''ArtifactType)
$(deriveSafeCopy 0 'base ''EnchantmentType)
$(deriveSafeCopy 0 'base ''LandType)
$(deriveSafeCopy 0 'base ''BasicLandType)
$(deriveSafeCopy 0 'base ''PlaneswalkerType)
$(deriveSafeCopy 0 'base ''SpellType)
$(deriveSafeCopy 0 'base ''CreatureType)
$(deriveSafeCopy 0 'base ''Rarity)
$(deriveSafeCopy 0 'base ''Border)
$(deriveSafeCopy 0 'base ''Cost)
$(deriveSafeCopy 0 'base ''Targets)
$(deriveSafeCopy 0 'base ''TargetMatch)
$(deriveSafeCopy 0 'base ''SpellMatch)
$(deriveSafeCopy 0 'base ''CardMatch)
$(deriveSafeCopy 0 'base ''Quality)
$(deriveSafeCopy 0 'base ''PermanentMatch)
$(deriveSafeCopy 0 'base ''ColorMatch)
$(deriveSafeCopy 0 'base ''BlockedStatus)
$(deriveSafeCopy 0 'base ''CombatStatus)
$(deriveSafeCopy 0 'base ''NonToken)
$(deriveSafeCopy 0 'base ''OwnControl)
$(deriveSafeCopy 0 'base ''PermanentTypeMatch)
$(deriveSafeCopy 0 'base ''Non)
$(deriveSafeCopy 0 'base ''PermanentStatus)
$(deriveSafeCopy 0 'base ''PermanentStatusMatch)
$(deriveSafeCopy 0 'base ''TapStatus)
$(deriveSafeCopy 0 'base ''FlipStatus)
$(deriveSafeCopy 0 'base ''FaceStatus)
$(deriveSafeCopy 0 'base ''PhaseStatus)
$(deriveSafeCopy 0 'base ''CountRange)
$(deriveSafeCopy 0 'base ''Count)
$(deriveSafeCopy 0 'base ''NumValue)
$(deriveSafeCopy 0 'base ''NumChange)
$(deriveSafeCopy 0 'base ''Duration)
$(deriveSafeCopy 0 'base ''Zone)
$(deriveSafeCopy 0 'base ''TriggerEvent)
$(deriveSafeCopy 0 'base ''Next)
$(deriveSafeCopy 0 'base ''PlayerMatch)
$(deriveSafeCopy 0 'base ''Step)
$(deriveSafeCopy 0 'base ''Divided)
$(deriveSafeCopy 0 'base ''FromAmong)
$(deriveSafeCopy 0 'base ''CardOrder)
$(deriveSafeCopy 0 'base ''Ability)
$(deriveSafeCopy 0 'base ''Effect)
$(deriveSafeCopy 0 'base ''Keyword)
$(deriveSafeCopy 0 'base ''Card)
$(deriveSafeCopy 0 'base ''SetType)
$(deriveSafeCopy 0 'base ''CardSet)

data CardDB = CardDB
            { _allCardSets      :: Map.Map SetCode CardSet
            , _allCards         :: IntMap.IntMap Card
            , _allMultiverseIDs :: Map.Map Name (Set MultiverseID)
            } deriving (Show, Typeable)

makeLenses ''CardDB

$(deriveSafeCopy 0 'base ''CardDB)

initialCardDB :: CardDB
initialCardDB = CardDB Map.empty IntMap.empty Map.empty

clearCardDB :: Update CardDB ()
clearCardDB = put initialCardDB

getCardSets :: Query CardDB [CardSet]
getCardSets = Map.elems . view allCardSets <$> ask

addCardSet :: CardSet -> Update CardDB ()
addCardSet cs = allCardSets %= Map.insert (cs^.code) cs

getCard :: MultiverseID -> Query CardDB (Maybe Card)
getCard i = IntMap.lookup i . view allCards <$> ask

getCardsByName :: Name -> Query CardDB [Card]
getCardsByName n = do
    is <- Map.findWithDefault Set.empty n . view allMultiverseIDs <$> ask
    cs <- view allCards <$> ask

    -- We can use fromJust here, because if an ID looked up in
    -- allMultiverseIDs isn't found in allCards, the integrity of
    -- the DB is in question, and we want to bail out
    return $ map (fromJust . (`IntMap.lookup` cs)) (Set.toList is)

getCards :: Query CardDB [Card]
getCards = IntMap.elems . view allCards <$> ask

addCard :: Card -> Update CardDB ()
addCard c = do
    allCards %= IntMap.insert (c^.multiverseID) c
    allMultiverseIDs %= Map.insertWith Set.union
                        (c^.name) (Set.singleton (c^.multiverseID))

makeAcidic ''CardDB [ 'clearCardDB
                    , 'getCardSets
                    , 'addCardSet
                    , 'getCard
                    , 'getCardsByName
                    , 'getCards
                    , 'addCard ]
