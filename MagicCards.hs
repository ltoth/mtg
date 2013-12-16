{-# LANGUAGE DeriveGeneric, FlexibleInstances, OverloadedStrings, TypeSynonymInstances #-}
import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List.Split (wordsBy)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Word (Word8)
import GHC.Generics

data Layout = Normal | Split | Flip | DoubleFaced | Token
              deriving (Show, Generic)
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
    parseJSON (String s) = return . map toManaSymbol $
      wordsBy (\c -> c == '{' || c == '}') (T.unpack s)
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
                               -- TODO: Add hybrid and phyrexian cases
                               -- FIXME: Catch no parse exception
                               _ -> CL $ read m
    parseJSON _ = fail "Could not parse mana cost"


data ManaSymbol = W | U | B | R | G | S | CL Word8 | X | Y | Z
                  | GW | WU | RW | WB | UB | GU | UR | BR | BG | RG
                  | W2 | U2 | B2 | R2 | G2 | WP | UP | BP | RP | GP | P
                  deriving (Show)

-- FIXME: This really should support decimals, because of Unhinged
type CMC = Word8

data Color = White | Blue | Black | Red | Green
             deriving (Show, Generic)
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
                 deriving (Show, Generic)
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
            deriving (Show, Generic)
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
              deriving (Show, Generic)
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
              deriving (Show, Generic)
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
          , border :: Maybe Border
          } deriving (Show)
instance FromJSON Card where
    parseJSON (Object v) = Card <$>
                           v .: "layout" <*>
                           v .: "type" <*>
                           v .: "types" <*>
                           v .: "colors" <*>
                           v .: "multiverseid" <*>
                           v .: "name" <*>
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

getCards :: IO (Either String [Card])
getCards =  eitherDecode <$> L.readFile "test2.json"

getLayout :: IO (Either String [Layout])
getLayout =  eitherDecode <$> L.readFile "test.json"

getManaCost :: IO (Either String [ManaCost])
getManaCost =  eitherDecode <$> L.readFile "test3.json"

main = return ()
