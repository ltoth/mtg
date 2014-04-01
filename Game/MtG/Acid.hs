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
            { allCardSets :: Map.Map SetCode CardSet }
            deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''CardDB)

initialDB :: CardDB
initialDB = CardDB { allCardSets = Map.empty }


getCardSets :: Query CardDB [CardSet]
getCardSets = Map.elems . allCardSets <$> ask

addCardSet :: CardSet -> Update CardDB ()
addCardSet cs = modify go
    where
      go (CardDB mcs) = CardDB $
        Map.insert (cs^.code) cs mcs

makeAcidic ''CardDB ['getCardSets, 'addCardSet]
