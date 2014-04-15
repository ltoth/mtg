module Debug where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Data.Lens (biplate)
import Data.Either
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Game.MtG.Types
import Game.MtG.CardTextParser (parseAndSetAbilities)
import Game.MtG.JSONParser (parseSet)

setFile :: String
setFile = "THS.json"

debugGetCards :: FilePath -> IO (Either String [Card])
debugGetCards fp = (view cards' <$>) <$> parseSet fp

filterCards :: (Card -> Bool) -> IO [Card]
filterCards p = go <$> debugGetCards setFile
  where go = rights .
             fmap parseAndSetAbilities .
             filter p .
             either (const []) id

cardTextIncludes :: Text -> Card -> Bool
cardTextIncludes s = fromMaybe False .
                     (view cardText >=> return . T.isInfixOf s)

nameStartsWith :: Text -> Card -> Bool
nameStartsWith s = T.isPrefixOf s . view name

---

-- map doesTarget <$> filterCards (nameStartsWith "Rescue")

doesTarget :: Card -> Bool
doesTarget c = anyOf biplate isTarget (c^.abilities)

isTarget :: Targets -> Bool
isTarget (Target _ _) = True
isTarget _            = False
