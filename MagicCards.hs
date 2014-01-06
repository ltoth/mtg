{-# LANGUAGE FlexibleInstances, NoMonomorphismRestriction, OverloadedStrings, TypeSynonymInstances #-}

module MagicCards
( Layout(..)
, Name
, ManaCost
, ManaSymbol(..)
, CMC
, Color(..)
, TypeLine
, Supertype(..)
, Type(..)
, Rarity(..)
, CardText
, Flavor
, Artist
, CardNumber
, Power
, Toughness
, Loyalty
, MultiverseID
, ImageName
, Watermark
, Border(..)
, Card(..)
, Ability(..)
, abilities -- FIXME: Move elsewhere?
, textToAbilities -- FIXME: This should probably not be exported
, manaCostParser -- FIXME: This should probably not be exported
, removeReminder -- FIXME: This should not be exported
, replaceThis -- FIXME: This should not be exported
, SetName
, SetCode
, SetRelease
, SetType
, SetBlock
, CardSet(..)

-- re-export types from MagicCards.Subtype
, Subtype(..)
, ArtifactType(..)
, EnchantmentType(..)
, LandType(..)
, BasicLandType(..)
, PlaneswalkerType(..)
, SpellType(..)
, CreatureType(..)
) where

import Control.Applicative
import Control.Monad
import Data.Aeson (FromJSON, parseJSON, Value(..), (.:), (.:?))
import Data.Char (isSpace)
import Data.Functor.Identity (Identity)
import Data.Int (Int8)
import Data.List.Split (wordsBy, splitOn)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Word (Word8)
import Text.Parsec.Prim (ParsecT)
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Text.Regex

import MagicCards.Subtype
import Text.Parsec.Char.Extra(ciChar, ciString)

data Layout = Normal | Split | Flip | DoubleFaced | TokenLayout | Plane | Scheme
            | Phenomenon
              deriving (Show, Eq)
instance FromJSON Layout where
    parseJSON (String s)
      | s == "normal" = return Normal
      | s == "split" = return Split
      | s == "flip" = return Flip
      | s == "double-faced" = return DoubleFaced
      | s == "token" = return TokenLayout
      | s == "plane" = return Plane
      | s == "scheme" = return Scheme
      | s == "phenomenon" = return Phenomenon
      | otherwise = fail "Invalid layout string specified"
    parseJSON _ = fail "Could not parse layout"

type Name = String

type ManaCost = [ManaSymbol]
instance FromJSON ManaCost where
    parseJSON (String s) = return . stringToManaCost $ T.unpack s
    parseJSON _ = fail "Could not parse mana cost"

-- TODO: Should this be a custom instance of Read instead?
stringToManaCost :: String -> ManaCost
stringToManaCost s = case (parse manaCostParser "" s) of
                      Left e -> error (show e)
                      Right xs -> xs

manaCostParser :: ParsecT String u Identity ManaCost
manaCostParser = many1 symbol
  where symbol = try (string "{G/W}" >> return GW)
             <|> try (string "{W/U}" >> return WU)
             <|> try (string "{R/W}" >> return RW)
             <|> try (string "{W/B}" >> return WB)
             <|> try (string "{U/B}" >> return UB)
             <|> try (string "{G/U}" >> return GU)
             <|> try (string "{U/R}" >> return UR)
             <|> try (string "{B/R}" >> return BR)
             <|> try (string "{B/G}" >> return BG)
             <|> try (string "{R/G}" >> return RG)
             <|> try (string "{2/W}" >> return W2)
             <|> try (string "{2/U}" >> return U2)
             <|> try (string "{2/B}" >> return B2)
             <|> try (string "{2/R}" >> return R2)
             <|> try (string "{2/G}" >> return G2)
             <|> try (string "{P/W}" >> return WP)
             <|> try (string "{P/U}" >> return UP)
             <|> try (string "{P/B}" >> return BP)
             <|> try (string "{P/R}" >> return RP)
             <|> try (string "{P/G}" >> return GP)
             <|> try (string "{W}" >> return W)
             <|> try (string "{U}" >> return U)
             <|> try (string "{B}" >> return B)
             <|> try (string "{R}" >> return R)
             <|> try (string "{G}" >> return G)
             <|> try (string "{S}" >> return S)
             <|> try (string "{X}" >> return X)
             <|> try (string "{Y}" >> return Y)
             <|> try (string "{Z}" >> return Z)
             <|> try (string "{P}" >> return P)
             <|> (do
                    char '{'
                    cl <- many1 digit
                    char '}'
                    return $ CL $ read cl)

data ManaSymbol = W | U | B | R | G | S | CL Word8 | X | Y | Z
                  | GW | WU | RW | WB | UB | GU | UR | BR | BG | RG
                  | W2 | U2 | B2 | R2 | G2 | WP | UP | BP | RP | GP | P
                  deriving (Show, Eq)

-- FIXME: This really should support decimals, because of Unhinged
type CMC = Word8

data Color = White | Blue | Black | Red | Green
             deriving (Show, Eq)
instance FromJSON Color where
    parseJSON (String s) = return . stringToColor $ T.unpack s
    parseJSON _ = fail "Could not parse color"

-- TODO: Should this be a custom instance of Read instead?
-- TODO: Generalize to take the parser fn as argument: parseString
stringToColor :: String -> Color
stringToColor s = case (parse colorParser "" s) of
                      Left e -> error (show e)
                      Right xs -> xs

colorParser :: ParsecT String u Identity Color
colorParser = try (ciString "White" >> return White)
          <|> try (ciString "Blue" >> return Blue)
          <|> try (ciString "Black" >> return Black)
          <|> try (ciString "Red" >> return Red)
          <|> try (ciString "Green" >> return Green)

type TypeLine = String

data Supertype = Basic | Legendary | Ongoing | Snow | World
                 deriving (Show, Eq)
instance FromJSON Supertype where
    parseJSON (String s) = return . stringToSupertype $ T.unpack s
    parseJSON _ = fail "Could not parse supertype"

-- TODO: Should this be a custom instance of Read instead?
-- TODO: Generalize to take the parser fn as argument: parseString
stringToSupertype :: String -> Supertype
stringToSupertype s = case (parse supertypeParser "" s) of
                      Left e -> error (show e)
                      Right xs -> xs

supertypeParser :: ParsecT String u Identity Supertype
supertypeParser = try (ciString "Basic" >> return Basic)
              <|> try (ciString "Legendary" >> return Legendary)
              <|> try (ciString "Ongoing" >> return Ongoing)
              <|> try (ciString "Snow" >> return Snow)
              <|> try (ciString "World" >> return World)

data Type = Instant | Sorcery | Artifact | Creature | Enchantment
            | Land | Planeswalker | Tribal
            deriving (Show, Eq)
instance FromJSON Type where
    parseJSON (String s) = return . stringToType $ T.unpack s
    parseJSON _ = fail "Could not parse type"

-- TODO: Should this be a custom instance of Read instead?
-- TODO: Generalize to take the parser fn as argument: parseString
stringToType :: String -> Type
stringToType s = case (parse typeParser "" s) of
                      Left e -> error (show e)
                      Right xs -> xs

typeParser :: ParsecT String u Identity Type
typeParser = try (ciString "Instant" >> return Instant)
         <|> try (ciString "Sorcery" >> return Sorcery)
         <|> try (ciString "Artifact" >> return Artifact)
         <|> try (ciString "Creature" >> return Creature)
         <|> try (ciString "Enchantment" >> return Enchantment)
         <|> try (ciString "Land" >> return Land)
         <|> try (ciString "Planeswalker" >> return Planeswalker)
         <|> try (ciString "Tribal" >> return Tribal)

data Rarity = Common | Uncommon | Rare | MythicRare | BasicLandRarity
              deriving (Show, Eq)
instance FromJSON Rarity where
    parseJSON (String s)
      | s == "Common" = return Common
      | s == "Uncommon" = return Uncommon
      | s == "Rare" = return Rare
      | s == "Mythic Rare" = return MythicRare
      | s == "Basic Land" = return BasicLandRarity
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
          , types :: [Type]
          , colors :: [Color]
          , multiverseid :: MultiverseID
          , name :: Name
          , names :: Maybe [Name]
          , supertypes :: Maybe [Supertype]
          , subtypes :: Maybe [Subtype]
          , cmc :: Maybe CMC
          , rarity :: Rarity
          , artist :: Artist
          , power :: Maybe Power
          , toughness :: Maybe Toughness
          , loyalty :: Maybe Loyalty
          , manaCost :: Maybe ManaCost
          , cardText :: Maybe CardText
          , cardNumber :: CardNumber
          , variations :: Maybe [MultiverseID]
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
                           v .:? "loyalty" <*>
                           v .:? "manaCost" <*>
                           v .:? "text" <*>
                           v .: "number" <*>
                           v .:? "variations" <*>
                           v .: "imageName" <*>
                           v .:? "watermark" <*>
                           v .:? "border"
    parseJSON _ = fail "Could not parse card"

data Cost = CMana ManaCost | CTap | CUntap | CLife Word8
          | CSacrifice [PermanentMatch]
          | CDiscardThis | CDiscard PermanentType -- FIXME: Should be more general,
          -- i.e. for discard two cards, etc.
          | CLoyalty LoyaltyCost | CRemoveCounter CountRange (Maybe CounterType)
          deriving (Show, Eq)

data PermanentMatch = PermanentMatch (Maybe CountRange) [Color] PermanentType [Ability] (Maybe Name) (Maybe OwnControl)
                    | ThisPermanent
                    deriving (Show, Eq)

data OwnControl = Own WhichPlayers | Control WhichPlayers
                deriving (Show, Eq)

data PermanentType = PermanentType (Maybe (Bool,Supertype)) (Maybe (Bool,Type)) (Maybe (Bool,Subtype))
                   | Token | Permanent
                   deriving (Show, Eq)

data CountRange = UpTo Count | Exactly Count | AtLeast Count
                deriving (Show, Eq)

-- FIXME: Should support X, so probably need new datatype, not just Word8
data Count = AnyCount Word8 | OtherCount Word8
           deriving (Show, Eq)

type CounterType = String

data LoyaltyCost = LC Int8 | LCMinusX deriving (Show, Eq)

data TriggerEvent = TEAt WhichPlayers Step | TEThisETB | TEThisLTB
                  | TEObjectETB PermanentType | TEObjectLTB PermanentType
                  | TEOther String -- FIXME: Make more value constr.
                  deriving (Show, Eq)

data WhichPlayers = EachPlayer | You | Opponents deriving (Show, Eq)

data Step = Untap | Upkeep | Draw | PreCombatMain
          | BeginningOfCombat | DeclareAttackers | DeclareBlockers
          | CombatDamage | EndOfCombat | PostCombatMain
          | End | Cleanup
          deriving (Show, Eq)

type TriggerCondition = String -- TODO: should this be the same as AltCostCondition?
type Effect = String
type ContinuousEffect = String
type ActivationInst = String
type AltCostCondition = String

data Ability = AdditionalCost ([Cost])
             | AlternativeCost ([Cost]) (Maybe [AltCostCondition])
             | KeywordAbility Keyword
             | ActivatedAbility ([Cost]) Effect (Maybe ActivationInst)
             | TriggeredAbility TriggerEvent Effect (Maybe [TriggerCondition])
             | StaticAbility ContinuousEffect
             | SpellAbility Effect
             deriving (Show, Eq)

-- TODO: Use arrows? to keep the reference to the card throughout,
-- as we need to refer to types etc.
abilities :: Card -> [Ability]
abilities c = fromMaybe [] $
              textToAbilities <$>
              removeReminder <$>
              replaceThis c

textToAbilities :: CardText -> [Ability]
textToAbilities t = case (parse paras "" t) of
                      Left e -> error (show e)
                      Right xs -> concat xs  -- flatten the list
  where paras = abilityPara `sepBy` (string "\n\n")
        abilityPara = try (keyword `sepBy1` commas)
                  <|> (optional abilityWord >>
                       many1 (try additional
                             <|> try alternative
                             <|> try alternativeFree
                             <|> try activated
                             <|> try triggered
                             <|> spell))
        commas = (try (string ", ") <|> string ",")
        abilityWord = aw >> string " — "
          where aw = try (string "Battalion")
                 <|> try (string "Bloodrush")
                 <|> try (string "Channel")
                 <|> try (string "Chroma")
                 <|> try (string "Domain")
                 <|> try (string "Fateful hour")
                 <|> try (string "Grandeur")
                 <|> try (string "Hellbent")
                 <|> try (string "Heroic")
                 <|> try (string "Imprint")
                 <|> try (string "Join forces")
                 <|> try (string "Kinship")
                 <|> try (string "Landfall")
                 <|> try (string "Metalcraft")
                 <|> try (string "Morbid")
                 <|> try (string "Radiance")
                 <|> try (string "Sweep")
                 <|> try (string "Tempting offer")
                 <|> try (string "Threshold")

        additional = ciString "As an additional cost to cast {This}, " >>
                            AdditionalCost <$> totalCost
                            <* optional (many (oneOf (". ")))

        alternative = do
          cond <- optionMaybe (string "If " *> conditions <* string ", ")
          ciString "You may "
          cost <- totalCost
          ciString " rather than pay {This}'s mana cost."
          return $ AlternativeCost cost cond

        alternativeFree = do
          cond <- optionMaybe (string "If " *> conditions <* string ", ")
          ciString "You may cast {This} without paying its mana cost."
          return $ AlternativeCost [] cond

        conditions = condition `sepBy1` condSet
        condSet = string " and " -- FIXME: This separation doesn't work now
        condition = many (noneOf ",\n")

        triggered =
          do
            event <- trigEvent
            string ", "
            cond <- optionMaybe $ try (string "if " *> conditions <* string ", ")
            effect <- many (noneOf "\n") -- TODO: sepEndBy1 try(". ") <|> "."
            return $ TriggeredAbility event effect cond

        trigEvent =
              try (ciString "At " >>
                    try (ciString "the beginning of " >>
                      TEAt <$> whichPlayers <*> step)
                    <|> try (ciString "the end of combat" >>
                      return (TEAt EachPlayer EndOfCombat))
                    <|> TEOther <$> many (noneOf ",\n"))
          <|> try (ciString "Whenever " >> TEOther <$> many (noneOf ",\n"))
          <|> try (ciString "When " >>
                    try (ciString "{This} enters the battlefield" >>
                      return TEThisETB)
                    <|> try (ciString "{This} leaves the battlefield" >>
                      return TEThisLTB)
                    <|> TEOther <$> many (noneOf ",\n"))

        -- TODO: Probably should parse non-possessives "each player" "you"
        -- the same for effects
        -- TODO: Also have to deal with "the next" and "it's controller's
        -- next"
        whichPlayers =
          try (    try (ciString "each player's ")
               <|> try (ciString "each player ")
            >> return EachPlayer)
          <|> try (try (ciString "your ")
               <|> try (ciString "you ")
            >> return You)
          <|> try (try (ciString "each opponent's ")
               <|> try (ciString "each opponent ")
               <|> try (ciString "opponent's ")
               <|> try (ciString "an opponent ")
               <|> try (ciString "your opponents' ")
               <|> try (ciString "your opponents ")
               <|> try (ciString "your opponent ")
               <|> try (ciString "all opponents' ")
               <|> try (ciString "all opponents ")
               <|> try (ciString "opponent ")
            >> return Opponents)
          <|> try (ciString "each " >> return EachPlayer)

        step =
          try (ciString "untap step" >> return Untap)
          <|> try (ciString "upkeep" >> return Upkeep)
          <|> try (ciString "draw step" >> return Draw)
          <|> try (ciString "precombat main phase" >> return PreCombatMain)
          <|> try (ciString "combat" >> return BeginningOfCombat)
          <|> try (ciString "declare attackers step" >> return DeclareAttackers)
          <|> try (ciString "declare blockers step" >> return DeclareBlockers)
          <|> try (ciString "postcombat main phase" >> return PostCombatMain)
          <|> try (ciString "end step" >> return End)
          <|> try (ciString "cleanup step" >> return Cleanup)

        -- FIXME: Replace "it" with "{This}" in some cases? How to tell?
        -- FIXME: Quoting: Witches' Eye - reuse abilityPara
        activated = do
          cost <- totalCost
          string ": "
          effect <- try ((noneOf "\n") `manyTill`
                        (try (string " Activate this ability only ")))
                    <|> many1 (noneOf "\n")
          instr <- optionMaybe (many1 (noneOf "\n"))
          return $ ActivatedAbility cost effect instr
        totalCost = abilityCost `sepBy1` abilityCostSep
        abilityCostSep = try (string ", and ")
                     <|> try (string ", ")
                     <|> try (string " and ")
                     <|> string ","
        abilityCost = try (ciString "Discard {This}" >> return CDiscardThis)
                  <|> try (string "{T}" >> return CTap)
                  <|> try (string "{Q}" >> return CUntap)
                  <|> try (do
                            ciString "Pay "
                            n <- many1 digit
                            string " life"
                            return $ CLife $ read n)
                  <|> try (do
                          ciString "Sacrifice "
                          ts <- targets
                          return $ CSacrifice ts)
                  <|> try (do
                        ciString "Remove "
                        n <- countRange
                        string " "
                        counterType <- optionMaybe (many1 (noneOf " \n"))
                        optional (string " ")
                        ciString "counter from {This}"
                        return $ CRemoveCounter n counterType)
                  <|> try ((optional (ciString "Pay ") >>
                        CMana <$> manaCostParser))
                  <|> try (string "-X" >> return (CLoyalty $ LCMinusX))
                  <|> (do
                        optional (char '+')
                        sign <- option "" (try (many1 (char '-')))
                        l <- many1 digit
                        return $ CLoyalty $ LC $ read $ sign ++ l)

        countRange = try (string "up to " >> (UpTo <$> countParser))
                 <|> try (string "at least " >> (AtLeast <$> countParser))
                 <|> try (Exactly <$> countParser)

        countParser = try (string "another" >> (return $ OtherCount $ 1))
                  <|> try (do
                          n <- numberParser
                          string " other"
                          return $ OtherCount n)
                  <|> try (AnyCount <$> numberParser)

        numberParser = try (choice [try (string "an"), try (string "a"),
                           try (string "one")] >> (return 1))
                  <|> try (string "two" >> (return 2))
                  <|> try (string "three" >> (return 3))
                  <|> try (string "four" >> (return 4))
                  <|> try (string "five" >> (return 5))
                  <|> try (string "six" >> (return 6))
                  <|> try (string "seven" >> (return 7))
                  <|> try (string "eight" >> (return 8))
                  <|> try (string "nine" >> (return 9))
                  <|> try (string "ten" >> (return 10))
                  <|> try (string "eleven" >> (return 11))
                  <|> try (string "twelve" >> (return 12))
                  <|> try (string "thirteen" >> (return 13))
                  <|> try (string "fourteen" >> (return 14))
                  <|> try (string "fifteen" >> (return 15))
                  <|> try (string "sixteen" >> (return 16))
                  <|> try (string "seventeen" >> (return 17))
                  <|> try (string "eighteen" >> (return 18))
                  <|> try (string "nineteen" >> (return 19))
                  <|> try (string "twenty" >> (return 20))

        -- FIXME: We should distinguish between "or" and "and" here
        -- Artisan's Sorrow, Swan Song, etc.
        targets = target `sepBy1` abilityCostSep

        target = try (string "{This}" >> return ThisPermanent)
             <|> try (do
                   n <- optionMaybe $ try countRange
                   unless (n == Nothing) (string " " >> return ())
                   -- TODO: support states like "attacking" "blocking"
                   -- Agrus Kos, Wojek Veteran
                   cs <- colorsParser
                   unless (cs == []) (string " " >> return ())
                   t <- permanentTypeParser
                   as <- withAbilities
                   cardName <- optionMaybe $ try cardNamed
                   oc <- optionMaybe $ try ownControl
                   return $ PermanentMatch n cs t as cardName oc)

        -- FIXME: Should we distinguish between "or" and "and" here?
        colorsParser = colorParser `sepBy` colorSep

        colorSep = try (string ", and ")
               <|> try (string ", or ")
               <|> try (string ", ")
               <|> try (string " and ")
               <|> try (string " or ")

        withAbilities = option [] (try (do
          string " with "
          keyword `sepBy1` abilityCostSep))

        ownControl = try (do
                         string " "
                         p <- whichPlayers
                         string "own"
                         optional (string "s")
                         return $ Own $ p)
                 <|> try (do
                         string " "
                         p <- whichPlayers
                         string "control"
                         optional (string "s")
                         return $ Control $ p)

        nonParser = option True (try (do
                                string "non"
                                optional (string "-")
                                return False))

        cardNamed = do
          string " named "
          many1 (noneOf ",:;.\n") -- FIXME: Actually match against
          -- possible card names, not just as strings, since
          -- this doesn't know when to stop properly, i.e. Kher Keep

        permanentTypeParser =
              try (string "permanent" >> optional (string "s") >>
                (return $ Permanent))
          <|> try (string "token" >> optional (string "s") >>
                (return $ Token))
          <|> try (do
                super <- optionMaybe $ try (do
                  non <- nonParser
                  t <- supertypeParser
                  return (non, t))
                sub <- optionMaybe $ try (do
                  non <- nonParser
                  t <- subtypeParser
                  return (non, t))
                t <- optionMaybe $ try (do
                  unless (super == Nothing && sub == Nothing)
                    (string " " >> return ())
                  non <- nonParser
                  t <- typeParser
                  return (non, t))
                optional (try (string " permanent"))
                optional (string "s") -- FIXME: deal with plural better
                return $ PermanentType super t sub)

        spell = SpellAbility <$> many1 (noneOf "\n")

        keyword = (ciString "Deathtouch" >> (return $ KeywordAbility Deathtouch))
              <|> (ciString "Defender" >> (return $ KeywordAbility Defender))
              <|> (ciString "Double strike" >> (return $ KeywordAbility DoubleStrike))
              <|> (ciString "Enchant player" >>
                    (return $ KeywordAbility $ Enchant (ETPlayer PTPlayer)))
              <|> (ciString "Enchant opponent" >>
                    (return $ KeywordAbility $ Enchant (ETPlayer PTOpponent)))
              -- TODO: pull these out into a separate parser for object types/characteristics
              <|> (ciString "Enchant creature" >>
                    (return $ KeywordAbility $ Enchant
                    (ETObject $ PermanentType Nothing (Just (True,Creature)) Nothing)))
              <|> (ciString "Enchant land" >>
                    (return $ KeywordAbility $ Enchant
                    (ETObject $ PermanentType Nothing (Just (True,Land)) Nothing)))
              <|> (ciString "Enchant artifact" >>
                    (return $ KeywordAbility $ Enchant
                    (ETObject $ PermanentType Nothing (Just (True,Artifact)) Nothing)))
              <|> (ciString "Enchant enchantment" >>
                    (return $ KeywordAbility $ Enchant
                    (ETObject $ PermanentType Nothing (Just (True,Enchantment)) Nothing)))
              <|> (ciString "Enchant permanent" >>
                    (return $ KeywordAbility $ Enchant ETPermanent))
              -- FIXME: Make Enchant type more specific: Chained to the
              -- Rocks, Aggression, Controlled Instincts
              <|> (ciString "Equip" >> keywordCostSep >>
                    (KeywordAbility . Equip) <$> totalCost)
              <|> (ciString "First strike" >> (return $ KeywordAbility FirstStrike))
              <|> (ciString "Flash" >> (return $ KeywordAbility Flash))
              <|> (ciString "Flying" >> (return $ KeywordAbility Flying))
              <|> (ciString "Haste" >> (return $ KeywordAbility Haste))
              <|> (ciString "Hexproof" >> (return $ KeywordAbility Hexproof))
              <|> (ciString "Indestructible" >> (return $ KeywordAbility Indestructible))
              <|> (ciString "Intimidate" >> (return $ KeywordAbility Intimidate))
              -- TODO: Parse Landwalk
              <|> (ciString "Lifelink" >> (return $ KeywordAbility Lifelink))
              -- TODO: Parse Protection
              <|> (ciString "Reach" >> (return $ KeywordAbility Reach))
              <|> (ciString "Shroud" >> (return $ KeywordAbility Shroud))
              <|> (ciString "Trample" >> (return $ KeywordAbility Trample))
              <|> (ciString "Vigilance" >> (return $ KeywordAbility Vigilance))
              <|> (ciString "Bestow" >> keywordCostSep >>
                    (KeywordAbility . Bestow) <$> totalCost)

        keywordCostSep = string " " <|> string "—"


data Keyword = Deathtouch
             | Defender
             | DoubleStrike
             | Enchant EnchantmentTarget
             | Equip ([Cost])
             | FirstStrike
             | Flash
             | Flying
             | Haste
             | Hexproof
             | Indestructible
             | Intimidate
             | Landwalk PermanentType
             | Lifelink
             | Protection (Either Quality PlayerType)
             | Reach
             | Shroud
             | Trample
             | Vigilance
             | Bestow ([Cost])
             deriving (Show, Eq)

-- TODO: Generalize this to [TargetingCharacteristic]
data EnchantmentTarget = ETObject PermanentType | ETPlayer PlayerType | ETPermanent
                       deriving (Show, Eq)

data PlayerType = PTPlayer | PTOpponent deriving (Show, Eq)

type Quality = String -- FIXME

removeReminder :: CardText -> CardText
-- FIXME: Should not be greedy
removeReminder t = subRegex (mkRegex " *\\([^)]+\\) *")
                   (subRegex (mkRegex "^\\([^)]+\\)\n\n") t "") ""

-- FIXME: THS Gods use just their "first" names to refer to {This} in their
-- text. Also Jarad in RTR. Need to be able to account for that, ideally
-- not with an explicit exception.
replaceThis :: Card -> Maybe CardText
replaceThis c = replace (name c) "{This}" <$> (cardText c)

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
