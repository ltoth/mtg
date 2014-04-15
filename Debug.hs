module Debug where

import Control.Applicative
import Control.Lens hiding (argument)
import Control.Monad
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

filterCards :: (Card -> Bool) -> IO [Either String Card]
filterCards p = go <$> debugGetCards setFile
  where go = fmap parseAndSetAbilities .
             filter p .
             either (const []) id

cardTextIncludes :: Text -> Card -> Bool
cardTextIncludes s = fromMaybe False .
                     (view cardText >=> return . T.isInfixOf s)

nameStartsWith :: Text -> Card -> Bool
nameStartsWith s = T.isPrefixOf s . view name
