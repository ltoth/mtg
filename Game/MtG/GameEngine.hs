module Game.MtG.GameEngine where

import Control.Lens
import qualified Data.IntMap as IntMap
import Data.Monoid
import qualified Data.Set as Set
import qualified Data.Text as T

import Game.MtG.Types

-- TODO: This should also be passed in [PlayerInfo] and
-- potentially decks :: [Card]
initialGame :: Int -> Game
initialGame n = Game
  { _players =
      zipWith
        (\i -> over playerInfo (<> (T.pack . show) i))
        [1..n]
        (replicate n initPlayer)
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
  where initPlayer = Player
          { _library = []
          , _hand = Set.empty
          , _graveyard = []
          , _life = 20
          , _poison = 0
          , _playerInfo = "Player "
          }
        initRelationships = Relationships
          { _attachedTo = IntMap.empty
          , _exiledWith = IntMap.empty
          }
