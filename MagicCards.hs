{-# LANGUAGE FlexibleInstances, OverloadedStrings, TypeSynonymInstances #-}

module MagicCards
( Layout(..)
, Name
, Names
, ManaCost
, ManaSymbol(..)
, CMC
, Color(..)
, Colors
, TypeLine
, Supertype(..)
, Supertypes
, Type(..)
, Types
, Subtype
, Subtypes
, Rarity(..)
, CardText
, Flavor
, Artist
, CardNumber
, Power
, Toughness
, Loyalty
, MultiverseID
, Variations
, ImageName
, Watermark
, Border(..)
, Card(..)
, cardText' -- FIXME: Move elsewhere?
, SetName
, SetCode
, SetRelease
, SetType
, SetBlock
, CardSet(..)
) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.List.Split (splitOn, wordsBy)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Word (Word8)

data Layout = Normal | Split | Flip | DoubleFaced | Token
              deriving (Show, Eq)
instance FromJSON Layout where
    parseJSON (String s)
      | s == "normal" = return Normal
      | s == "split" = return Split
      | s == "flip" = return Flip
      | s == "double-faced" = return DoubleFaced
      | s == "token" = return Token
      | otherwise = fail "Invalid layout string specified"
    parseJSON _ = fail "Could not parse layout"

type Name = String

type Names = [Name]

type ManaCost = [ManaSymbol]
instance FromJSON ManaCost where
    parseJSON (String s) = return . stringToManaCost $ T.unpack s
    parseJSON _ = fail "Could not parse mana cost"

-- TODO: Should this be a custom instance of Read instead?
-- We'd have to implement readsPrec
stringToManaCost :: String -> ManaCost
stringToManaCost s = map toManaSymbol $
      wordsBy (\c -> c == '{' || c == '}') s
      where toManaSymbol m = case m of
                               "W" -> W
                               "U" -> U
                               "B" -> B
                               "R" -> R
                               "G" -> G
                               "S" -> S
                               "X" -> X
                               "Y" -> Y
                               "Z" -> Z
                               "G/W" -> GW
                               "W/U" -> WU
                               "R/W" -> RW
                               "W/B" -> WB
                               "U/B" -> UB
                               "G/U" -> GU
                               "U/R" -> UR
                               "B/R" -> BR
                               "B/G" -> BG
                               "R/G" -> RG
                               "2/W" -> W2
                               "2/U" -> U2
                               "2/B" -> B2
                               "2/R" -> R2
                               "2/G" -> G2
                               "P/W" -> WP
                               "P/U" -> UP
                               "P/B" -> BP
                               "P/R" -> RP
                               "P/G" -> GP
                               "P" -> P
                               -- FIXME: Catch no parse exception
                               _ -> CL $ read m

data ManaSymbol = W | U | B | R | G | S | CL Word8 | X | Y | Z
                  | GW | WU | RW | WB | UB | GU | UR | BR | BG | RG
                  | W2 | U2 | B2 | R2 | G2 | WP | UP | BP | RP | GP | P
                  deriving (Show, Eq)

-- FIXME: This really should support decimals, because of Unhinged
type CMC = Word8

data Color = White | Blue | Black | Red | Green
             deriving (Show, Eq)
instance FromJSON Color where
    parseJSON (String s)
      | s == "White" = return White
      | s == "Blue" = return Blue
      | s == "Black" = return Black
      | s == "Red" = return Red
      | s == "Green" = return Green
      | otherwise = fail "Invalid color string specified"
    parseJSON _ = fail "Could not parse color"

type Colors = [Color]

type TypeLine = String

-- FIXME: There might be more possible values
data Supertype = Basic | Legendary | Snow | World | Tribal
                 deriving (Show, Eq)
instance FromJSON Supertype where
    parseJSON (String s)
      | s == "Basic" = return Basic
      | s == "Legendary" = return Legendary
      | s == "Snow" = return Snow
      | s == "World" = return World
      | s == "Tribal" = return Tribal
      | otherwise = fail "Invalid supertype string specified"
    parseJSON _ = fail "Could not parse supertype"

type Supertypes = [Supertype]

data Type = Instant | Sorcery | Artifact | Creature | Enchantment
            | Land | Planeswalker
            deriving (Show, Eq)
instance FromJSON Type where
    parseJSON (String s)
      | s == "Instant" = return Instant
      | s == "Sorcery" = return Sorcery
      | s == "Artifact" = return Artifact
      | s == "Creature" = return Creature
      | s == "Enchantment" = return Enchantment
      | s == "Land" = return Land
      | s == "Planeswalker" = return Planeswalker
      | otherwise = fail "Invalid type string specified"
    parseJSON _ = fail "Could not parse type"

type Types = [Type]

type Subtype = String

type Subtypes = [Subtype]

data Rarity = Common | Uncommon | Rare | MythicRare | BasicLand
              deriving (Show, Eq)
instance FromJSON Rarity where
    parseJSON (String s)
      | s == "Common" = return Common
      | s == "Uncommon" = return Uncommon
      | s == "Rare" = return Rare
      | s == "Mythic Rare" = return MythicRare
      | s == "Basic Land" = return BasicLand
      | otherwise = fail "Invalid rarity specified"
    parseJSON _ = fail "Could not parse rarity"

type CardText = String

type Flavor = String

type Artist = String

type CardNumber = String

type Power = String

type Toughness = String

type Loyalty = Word8

type MultiverseID = Integer

type Variations = [MultiverseID]

type ImageName = String

type Watermark = String

data Border = BlackBorder | WhiteBorder | SilverBorder
              deriving (Show, Eq)
instance FromJSON Border where
    parseJSON (String s)
      | s == "black" = return BlackBorder
      | s == "white" = return WhiteBorder
      | s == "silver" = return SilverBorder
      | otherwise = fail "Invalid border specified"
    parseJSON _ = fail "Could not parse border"

data Card = Card
          { layout :: Layout
          , typeLine :: TypeLine
          , types :: Types
          , colors :: Colors
          , multiverseid :: MultiverseID
          , name :: Name
          , names :: Maybe [Name]
          , supertypes :: Maybe Supertypes
          , subtypes :: Maybe Subtypes
          , cmc :: Maybe CMC
          , rarity :: Rarity
          , artist :: Artist
          , power :: Maybe Power
          , toughness :: Maybe Toughness
          , manaCost :: Maybe ManaCost
          , cardText :: Maybe CardText
          , cardNumber :: CardNumber
          , imageName :: ImageName
          , watermark :: Maybe Watermark
          , cardBorder :: Maybe Border
          } deriving (Show)
instance FromJSON Card where
    parseJSON (Object v) = Card <$>
                           v .: "layout" <*>
                           v .: "type" <*>
                           v .: "types" <*>
                           v .: "colors" <*>
                           v .: "multiverseid" <*>
                           v .: "name" <*>
                           v .:? "names" <*>
                           v .:? "supertypes" <*>
                           v .:? "subtypes" <*>
                           v .:? "cmc" <*>
                           v .: "rarity" <*>
                           v .: "artist" <*>
                           v .:? "power" <*>
                           v .:? "toughness" <*>
                           v .:? "manaCost" <*>
                           v .:? "text" <*>
                           v .: "number" <*>
                           v .: "imageName" <*>
                           v .:? "watermark" <*>
                           v .:? "border"
    parseJSON _ = fail "Could not parse card"

cardText' :: Card -> Maybe CardText
cardText' c = replace (name c) "{This}" <$> (cardText c)

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new = intercalate new . splitOn old

type SetName = String
type SetCode = String

-- TODO: Should be UTCTime or something?
type SetRelease = String

data SetType = Core | Expansion | Reprint | Box | Un | FromTheVault
               | PremiumDeck | DuelDeck | Starter | Commander
               | Planechase | Archenemy | Promo
              deriving (Show, Eq)
instance FromJSON SetType where
    parseJSON (String s)
      | s == "core" = return Core
      | s == "expansion" = return Expansion
      | s == "reprint" = return Reprint
      | s == "box" = return Box
      | s == "un" = return Un
      | s == "from the vault" = return FromTheVault
      | s == "premium deck" = return PremiumDeck
      | s == "duel deck" = return DuelDeck
      | s == "starter" = return Starter
      | s == "commander" = return Commander
      | s == "planechase" = return Planechase
      | s == "archenemy" = return Archenemy
      | s == "promo" = return Promo
      | otherwise = fail "Invalid set type specified"
    parseJSON _ = fail "Could not parse set type"

type SetBlock = String

data CardSet = CardSet
             { setName :: SetName
             , code :: SetCode
             , release :: SetRelease
             , border :: Border
             , setType :: SetType
             , block :: Maybe SetBlock
             , cards :: [Card]
             } deriving (Show)
instance FromJSON CardSet where
    parseJSON (Object v) = CardSet <$>
                           v .: "name" <*>
                           v .: "code" <*>
                           v .: "releaseDate" <*>
                           v .: "border" <*>
                           v .: "type" <*>
                           v .:? "block" <*>
                           v .: "cards"
    parseJSON _ = fail "Could not parse card set"
