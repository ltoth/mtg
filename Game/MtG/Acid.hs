{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Game.MtG.Acid where

import Control.Applicative
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.Data
import qualified Data.Map as Map
import Data.SafeCopy

import Game.MtG.Types

$(deriveSafeCopy 0 'base ''Border)
$(deriveSafeCopy 0 'base ''SetType)
$(deriveSafeCopy 0 'base ''CardSet)

data CardDB = CardDB
            { _allCardSets :: Map.Map SetCode CardSet }
            deriving (Show, Typeable)

makeLenses ''CardDB

$(deriveSafeCopy 0 'base ''CardDB)

initialCardDB :: CardDB
initialCardDB = CardDB Map.empty

clearCardDB :: Update CardDB ()
clearCardDB = put initialCardDB

getCardSets :: Query CardDB [CardSet]
getCardSets = Map.elems . view allCardSets <$> ask

addCardSet :: CardSet -> Update CardDB ()
addCardSet cs = allCardSets %= Map.insert (cs^.code) cs

makeAcidic ''CardDB [ 'clearCardDB
                    , 'getCardSets
                    , 'addCardSet ]
