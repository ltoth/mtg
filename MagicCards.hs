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
import Text.ParserCombinators.Parsec.Extra (sepBy2)

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
             <|> try (string "{Y}" >> return Y) -- TODO: Only Unhinged,
             -- so get rid of it?
             <|> try (string "{Z}" >> return Z) -- TODO: Only Unhinged
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

data Cost = CMana ManaCost | CTap | CUntap
          | CEffect Effect
          | CDiscardThis | CDiscard [CardMatch] -- FIXME: Should be more general,
          -- i.e. for discard two cards, etc.
          | CLoyalty LoyaltyCost | CRemoveCounter CountRange (Maybe CounterType)
          deriving (Show, Eq)

data Targets = Target CountRange [TargetMatch]
             | NoTarget (Maybe CountRange) [TargetMatch]
             deriving (Show, Eq)

-- FIXME: Support targeting zones ("target library" Circu, Dimir Lobotomist)
-- § 114.1 "targets are object(s), player(s), and/or zone(s)"
-- § 109.1 "an object is an ability on the stack, a card, a copy of a card,
-- a token, a spell, a permanent or an emblem"
--
-- However, an emblem is not a valid target
data TargetMatch = TMPermanent PermanentMatch | TMSpell SpellMatch
                 | TMCard CardMatch | TMPlayer PlayerMatch
                 | TMThis | TMEnchantedPermanent | TMIt | TMThey
                 deriving (Show, Eq)

-- TODO: for targeting spells (i.e. counterspells)
type SpellMatch = String

-- TODO: for targeting/choosing cards (in graveyards, hands, libraries?)
type CardMatch = String

-- FIXME: This is for the protection keyword
type Quality = String

-- TODO: Ability should be Non Ability ("with" vs. "without")
data PermanentMatch = PermanentMatch (Maybe BlockedStatus)
                        [CombatStatus] ColorMatch
                        NonToken PermanentTypeMatch [Ability] (Maybe Name)
                        (Maybe OwnControl)
                    deriving (Show, Eq)

data ColorMatch = CMColors [Non Color] | CMMonocolored | CMMulticolored
                deriving (Show, Eq)

data BlockedStatus = Blocked | Unblocked
                   deriving (Show, Eq)

data CombatStatus = Attacking | Blocking
                  deriving (Show, Eq)

data NonToken = NonToken | CardOrToken
              deriving (Show, Eq)

data OwnControl = Own PlayerMatch | Control PlayerMatch
                deriving (Show, Eq)

data PermanentTypeMatch = PermanentTypeMatch [Non Supertype] [Non Type]
                            [Non Subtype]
                        | Token | Permanent
                        deriving (Show, Eq)

data Non a = Non Bool a
           deriving (Show, Eq)

data PermanentStatus =
    PermanentStatus TapStatus FlipStatus FaceStatus PhaseStatus
    deriving (Show, Eq)

data PermanentStatusMatch =
    PermanentStatusMatch (Maybe TapStatus) (Maybe FlipStatus) (Maybe FaceStatus) (Maybe PhaseStatus)
    deriving (Show, Eq)

data TapStatus = Tapped | Untapped
               deriving (Show, Eq)

data FlipStatus = Flipped | Unflipped
                deriving (Show, Eq)

data FaceStatus = FaceUp | FaceDown
                deriving (Show, Eq)

data PhaseStatus = PhasedIn | PhasedOut
                 deriving (Show, Eq)

data CountRange = UpTo Count | Exactly Count | AtLeast Count
                | OneOf [NumValue] | AnyNumber | Other
                deriving (Show, Eq)

data Count = AnyCount NumValue | OtherCount NumValue
           deriving (Show, Eq)

data NumValue = NumValue Word8 | NumValueX | All | NumVariable Calculation
              deriving (Show, Eq)

-- FIXME: Actually parse calculations properly
type Calculation = String

type CounterType = String

data LoyaltyCost = LC Int8 | LCMinusX deriving (Show, Eq)

data TriggerEvent = TEAt PlayerMatch Step | TEThisETB | TEThisLTB
                  | TEThisETBOrDies | TEThisDies
                  | TEObjectETB PermanentMatch
                  | TEObjectLTB PermanentMatch
                  | TEOther String -- FIXME: Make more value constr.
                  deriving (Show, Eq)

data PlayerMatch = EachPlayer | You | Player | Opponent | Opponents
                 | Controller | Owner
                 deriving (Show, Eq)

data Step = UntapStep | Upkeep | DrawStep | PreCombatMain
          | BeginningOfCombat | DeclareAttackers | DeclareBlockers
          | CombatDamage | EndOfCombat | PostCombatMain
          | End | Cleanup
          deriving (Show, Eq)

type TriggerCondition = String -- TODO: should this be the same as AltCostCondition?
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

data Effect =
    -- One-shot effects
    Destroy Targets
    | Exile Targets (Maybe TriggerEvent)
    | Tap Targets
    | Untap Targets
    | LoseLife PlayerMatch NumValue
    | GainLife PlayerMatch NumValue
    | PayLife NumValue
    | HaveAbilities Targets [Ability]
    | DrawCard Targets NumValue
    | Sacrifice Targets

    -- TODO: Parse "for each" multipliers, which can
    -- be at the beginning (Curse of the Swine) or end
    -- of the effect (Nemesis of Mortals)

    -- Other effects
    | OptionalEffect PlayerMatch Effect
    | OtherEffect String
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
                      -- FIXME: Perhaps we shouldn't flatten the list, so
                      -- that when Artisan's Sorrow has an illegal target,
                      -- we know not to resolve Scry 2. Those effects are
                      -- one ability.
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
            e <- effect
            return $ TriggeredAbility event e cond

        trigEvent =
              try (ciString "At " >>
                    -- TODO: first check "at the beginning of combat
                    -- on your turn" (Battle-Rattle Shaman)
                    -- TODO: first check "at the beginning of each of
                    -- your main phases" (Carpet of Flowers)
                    try (ciString "the beginning of " >>
                      TEAt <$> playerMatch <*> step)
                    <|> try (ciString "the end of combat" >>
                      return (TEAt EachPlayer EndOfCombat))
                    <|> TEOther <$> many (noneOf ",\n"))
          <|> try (ciString "Whenever " >> TEOther <$> many (noneOf ",\n"))
          <|> try (ciString "When " <|> ciString " until " >>
                    -- TODO: Should really return OrList [TrigEvent]
                    -- for cards like Ashen Rider, Absolver Thrull
                    -- TODO: Should use permanentMatch to match what
                    -- ETBs/dies/LTBs, and consolidate the This value
                    -- constructors with the Other
                    try (ciString "{This} enters the battlefield or dies" >>
                      return TEThisETBOrDies)
                    <|> try (ciString "{This} enters the battlefield" >>
                      return TEThisETB)
                    <|> try (ciString "{This} dies" >>
                      return TEThisDies)
                    <|> try (ciString "{This} leaves the battlefield" >>
                      return TEThisLTB)
                    <|> TEOther <$> many (noneOf ",\n"))

        -- TODO: Also have to deal with "the next" and "it's controller's
        -- next" and "your next"
        playerMatch =
          (try (   try (ciString "each player's")
               <|> try (ciString "each player")
            >> return EachPlayer)
          <|> try (try (ciString "your")
               <|> try (ciString "you")
            >> return You)
          <|> try (try (ciString "each opponent's")
               <|> try (ciString "each opponent")
               <|> try (ciString "your opponents'")
               <|> try (ciString "your opponents")
               <|> try (ciString "all opponents'")
               <|> try (ciString "all opponents")
               <|> try (ciString "each other player's") -- FIXME: Not sure if
               -- it always means opponents, but it does on Prophet of Kruphix
               <|> try (ciString "each other player")
            >> return Opponents)
          <|> try (try (ciString "opponent's")
               <|> try (ciString "an opponent")
               <|> try (ciString "your opponent")
               <|> try (ciString "opponent")
            >> return Opponent)
          <|> try (try (ciString "player's")
               <|> try (ciString "player")
               <|> try (ciString "any player's") -- FIXME: Perhaps this should be
               -- a separate value constructor, i.e. AnyPlayer
               <|> try (ciString "any player")
            >> return Player)
          <|> try (try (ciString "its controller's")
               <|> try (ciString "its controller")
               <|> try (ciString "their controllers'") -- FIXME: Perhaps separate?
               <|> try (ciString "their controllers")
               <|> try (ciString "their controller's")
               <|> try (ciString "their controller")
            >> return Controller)
          <|> try (try (ciString "its owner's")
               <|> try (ciString "its owner")
               <|> try (ciString "their owners'") -- FIXME: Perhaps separate?
               <|> try (ciString "their owners")
               <|> try (ciString "their owner's")
               <|> try (ciString "their owner")
            >> return Owner)
          <|> try (ciString "each" >> return EachPlayer))
          <* optional (string " ")

        step =
          try (ciString "untap step" >> return UntapStep)
          <|> try (ciString "upkeep" >> return Upkeep)
          <|> try (ciString "draw step" >> return DrawStep)
          <|> try (ciString "precombat main phase" >> return PreCombatMain)
          <|> try (ciString "combat" >> return BeginningOfCombat)
          <|> try (ciString "declare attackers step" >> return DeclareAttackers)
          <|> try (ciString "declare blockers step" >> return DeclareBlockers)
          <|> try (ciString "postcombat main phase" >> return PostCombatMain)
          <|> try (ciString "end step" >> return End)
          <|> try (ciString "cleanup step" >> return Cleanup)

        effect = (try (OptionalEffect <$> playerMatch
                         <*> (try $ ciString "may " *> effect))
              <|> try (ciString "destroy" >> optional (string "s")
                         >> string " " >> Destroy <$> targets)
              <|> try (ciString "exile" >> optional (string "s")
                         >> string " " >> Exile <$> targets
                         <*> optionMaybe trigEvent)
              <|> try (ciString "tap " >> Tap <$> targets)
              <|> try (ciString "untap " >> Untap <$> targets)
              <|> try (LoseLife <$> playerMatch
                         <*> (try $ (ciString "lose" >> optional (string "s")
                             >> string " ") *> numberParser
                             <* (optional (string " ") >> string "life")))
              <|> try (GainLife <$> playerMatch
                         <*> (try $ (ciString "gain" >> optional (string "s")
                             >> string " ") *> numberParser
                             <* (optional (string " ") >> string "life")))
              <|> try (PayLife <$> (ciString "Pay " *> numberParser
                             <* (optional (string " ") >> string "life")))
              <|> try (HaveAbilities <$> targets
                         <*> (try $ (optional (string " ") >>
                             try (ciString "have") <|> (ciString "has")
                             >> string " ") *> (keyword `sepBy1` andSep)))
              <|> try (DrawCard <$> option (NoTarget Nothing [TMPlayer You]) targets
                         <*> (try $ (ciString "draw" >> optional (string "s")
                             >> string " ") *> numberParser
                             <* (optional (string " ") >> string "card"
                             >> optional (string "s"))))
              <|> try (ciString "Sacrifice " >> Sacrifice <$> targets)
              <|> (OtherEffect <$> many1 (noneOf ".\n"))
              ) <* optional (numVariableConsume)
              <* optional (string ".") <* optional (string " ")

        -- FIXME: Quoting: Witches' Eye - reuse abilityPara
        activated = do
          cost <- totalCost
          string ": "
          e <- try (effect <* (try (string " Activate this ability only ")))
               <|> effect
          instr <- optionMaybe (many1 (noneOf "\n"))
          return $ ActivatedAbility cost e instr
        totalCost = abilityCost `sepBy1` andSep
        andSep = try (string ", and ")
                     <|> try (string ", ")
                     <|> try (string " and ")
                     <|> string ","  -- FIXME: Is this necessary?
        abilityCost = try (ciString "Discard {This}" >> return CDiscardThis)
                  <|> try (string "{T}" >> return CTap)
                  <|> try (string "{Q}" >> return CUntap)
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
                  <|> try (CEffect <$> effect)

        countRange = try (ciString "any number of" >> return AnyNumber)
                 <|> try (ciString "other" >> return Other)
                 <|> try (ciString "up to " >> (UpTo <$> countParser))
                 <|> try (ciString "at least " >> (AtLeast <$> countParser))
                 <|> try (AtLeast <$> countParser <* string " or greater")
                 <|> try (AtLeast <$> countParser <* string " or more")
                 <|> try (UpTo <$> countParser <* string " or less")
                 <|> try (OneOf <$> (numberParser `sepBy2` orSep))
                 <|> try (Exactly <$> countParser)

        countParser = try (string "another" >>
                          (return $ (OtherCount . NumValue) $ 1))
                  <|> try (do
                          n <- numberParser
                          string " other"
                          return $ OtherCount n)
                  <|> try (AnyCount <$> numberParser)

        orSep = try (string ", or ")
            <|> try (string ", ")
            <|> try (string " or ")

        numberParser = try (string "all" >> (return All))
                  <|> try (do
                          try (string "X")
                          lookAhead $ try (do
                            noneOf ("\n") `manyTill` try (string ", where X is ")
                            NumVariable <$> many1 (noneOf (".\n"))))
                  <|> try (do
                          optional $ try (string "a number of")
                          optional $ try (string "an amount of")
                          lookAhead $ try (do
                            noneOf ("\n") `manyTill` try (string "equal to ")
                            NumVariable <$> many1 (noneOf (".\n"))))
                  <|> try (string "an" >> (return $ NumValue 1))
                  <|> try (string "a" >> (return $ NumValue 1))
                  <|> try (string "both" >> (return $ NumValue 2))
                  <|> try (string "eleven" >> (return $ NumValue 11))
                  <|> try (string "twelve" >> (return $ NumValue 12))
                  <|> try (string "thirteen" >> (return $ NumValue 13))
                  <|> try (string "fourteen" >> (return $ NumValue 14))
                  <|> try (string "fifteen" >> (return $ NumValue 15))
                  <|> try (string "sixteen" >> (return $ NumValue 16))
                  <|> try (string "seventeen" >> (return $ NumValue 17))
                  <|> try (string "eighteen" >> (return $ NumValue 18))
                  <|> try (string "nineteen" >> (return $ NumValue 19))
                  <|> try (string "twenty" >> (return $ NumValue 20))
                  <|> try (string "one" >> (return $ NumValue 1))
                  <|> try (string "two" >> (return $ NumValue 2))
                  <|> try (string "three" >> (return $ NumValue 3))
                  <|> try (string "four" >> (return $ NumValue 4))
                  <|> try (string "five" >> (return $ NumValue 5))
                  <|> try (string "six" >> (return $ NumValue 6))
                  <|> try (string "seven" >> (return $ NumValue 7))
                  <|> try (string "eight" >> (return $ NumValue 8))
                  <|> try (string "nine" >> (return $ NumValue 9))
                  <|> try (string "ten" >> (return $ NumValue 10))
                  <|> try (string "X" >> (return NumValueX))
                  <|> try ((NumValue . read) <$> (many1 digit))

        -- used to consume this input, normally seen using lookAhead
        numVariableConsume =
              try (string " equal to " >> many1 (noneOf (".\n")))
          <|> try (string ", where X is " >> many1 (noneOf (".\n")))

        -- FIXME: We should distinguish between "or" and "and" here
        -- Artisan's Sorrow, Swan Song, Corrupted Roots etc., Hero's
        -- Downfall
        targets =
          try (do
              n <- option (Exactly (AnyCount (NumValue 1)))
                   (try $ countRange <* string " ")
              ciString "target "
              tms <- targetMatch `sepBy1` andOrSep' --FIXME: deal with spaces better
              return $ Target n tms)
          <|> try (do
              n <- optionMaybe (try $ countRange <* string " ")
              tms <- targetMatch `sepBy1` andOrSep'
              return $ NoTarget n tms)

        -- FIXME: Remove this and deal with consuming trailing spaces
        -- in permanentTypeParser better
        andOrSep' = try (string ", and ")
               <|> try (string ", or ")
               <|> try (string ", ")
               <|> try (string "and ")
               <|> try (string "or ")

        targetMatch = try it
                  <|> try they
                  <|> try enchanted
                  <|> try (string "{This}" >> optional (string " ")
                        >> return TMThis)
                  <|> try (TMPlayer <$> playerMatch)
                  <|> try (TMPermanent <$> permanentMatch)

        -- FIXME: Make this more robust
        it = (try (ciString "that card")
          <|> try (string "that " <* permanentTypeParser)
          <|> try (ciString "it")
          >> return TMIt)
          <* optional (string " ")  -- to match permanentType's behavior
          -- of consuming trailing spaces

        -- FIXME: Make this more robust
        they = (try (ciString "those cards")
          <|> try (string "those " <* permanentTypeParser)
          <|> try (ciString "they")
          >> return TMThey)
          <* optional (string " ")  -- to match permanentType's behavior
          -- of consuming trailing spaces

        enchanted = (try (ciString "enchanted " <* permanentTypeParser)
          >> return TMEnchantedPermanent)

        permanentMatch = try (do
          -- These are necessary for "a creature, a land, and a Wall"
          optional (try $ string "an ")
          optional (try $ string "a ")

          b <- optionMaybe $ try blocked
          combat <- combatStatuses
          cs <- colorMatch
          nt <- nonToken
          t <- permanentTypeParser
          as <- withAbilities
          -- TODO: support "with [other quality]", e.g.
          -- "power 4 or greater", "CMC 3 or less"
          -- Elspeth, Abrupt Decay
          -- as well as "with(out) a fate counter on it"
          -- Oblivion Stone
          cardName <- optionMaybe $ try cardNamed
          oc <- optionMaybe $ try ownControl
          -- TODO: support other conditions like
          -- "that dealt damage to you this turn"
          -- Spear of Heliod
          return $ PermanentMatch b combat cs nt t as cardName oc)

        blocked = try (ciString "blocked " >> return Blocked)
              <|> try (ciString "unblocked " >> return Unblocked)

        combatStatuses = combatStatus `sepBy` orSep
                         <* optional (string " ")

        combatStatus = try (ciString "attacking" >> return Attacking)
                   <|> try (ciString "blocking" >> return Blocking)

        colorMatch =
          (try (ciString "colorless" >>
                   return (CMColors [Non False White, Non False Blue,
                                   Non False Black, Non False Red,
                                   Non False Green]))
           <|> try (ciString "monocolored" >> return CMMonocolored)
           <|> try (ciString "multicolored" >> return CMMulticolored)
           <|> try ((ciString "colored" <|> ciString "all colors") >>
                   return (CMColors [Non True White, Non True Blue,
                                   Non True Black, Non True Red,
                                   Non True Green]))
           -- FIXME: Should we distinguish between "or" and "and" here?
           <|> try (CMColors <$> (try (Non <$> nonParser <*> colorParser)
           `sepBy` andOrSep))
          ) <* optional (string " ")

        andOrSep = try (string ", and ")
               <|> try (string ", or ")
               <|> try (string ", ")
               <|> try (string " and ")
               <|> try (string " or ")

        nonToken = option CardOrToken $
                   try (string "nontoken " >> return NonToken)

        -- FIXME: Deal with "without", probably using Non type
        withAbilities = option [] (try (do
          optional (string " ")
          string "with "
          keyword `sepBy1` andSep))

        ownControl = try (do
                         optional (string " ")
                         p <- playerMatch
                         string "own"
                         optional (string "s")
                         return $ Own $ p)
                 <|> try (do
                         optional (string " ")
                         p <- playerMatch
                         string "control"
                         optional (string "s")
                         return $ Control $ p)

        nonParser = option True (try (do
                                string "non"
                                optional (string "-")
                                return False))

        cardNamed = do
          optional (string " ")
          string "named "
          many1 (noneOf ",:;.\n") -- FIXME: Actually match against
          -- possible card names, not just as strings, since
          -- this doesn't know when to stop properly, i.e. Kher Keep

        permanentTypeParser =
              try (string "permanent" >> optional (string "s") >>
                (return $ Permanent))
          <|> try (string "token" >> optional (string "s") >>
                (return $ Token))
          <|> try (do
                super <- ((try (Non <$> nonParser <*> supertypeParser))
                        `sepEndBy` (string " "))
                sub <- ((try (Non <$> nonParser <*> subtypeParser))
                      `sepEndBy` (string " "))
                t <- ((try (Non <$> nonParser <*> typeParser))
                    `sepEndBy` (string " "))
                optional (string " ")
                optional (try (string "permanent"))
                optional (string "s") -- FIXME: deal with plural better
                when (super == [] && sub == [] && t == [])
                  (fail "Did not match any permanent type")
                return $ PermanentTypeMatch super t sub)

        spell = SpellAbility <$> effect

        keyword = (ciString "Deathtouch" >> (return $ KeywordAbility Deathtouch))
              <|> (ciString "Defender" >> (return $ KeywordAbility Defender))
              <|> (ciString "Double strike" >> (return $ KeywordAbility DoubleStrike))
              <|> (ciString "Enchant " >>
                    (KeywordAbility . Enchant .
                      (Target (Exactly (AnyCount (NumValue 1)))))
                    <$> (targetMatch `sepBy1` orSep))
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
             | Enchant Targets
             | Equip ([Cost])
             | FirstStrike
             | Flash
             | Flying
             | Haste
             | Hexproof
             | Indestructible
             | Intimidate
             | Landwalk PermanentTypeMatch
             | Lifelink
             | Protection (Either Quality PlayerMatch)
             | Reach
             | Shroud
             | Trample
             | Vigilance
             | Bestow ([Cost])
             deriving (Show, Eq)

removeReminder :: CardText -> CardText
-- FIXME: Should not be greedy
removeReminder t = subRegex (mkRegex " *\\([^)]+\\) *")
                   (subRegex (mkRegex "^\\([^)]+\\)\n\n") t "") ""

replaceThis :: Card -> Maybe CardText
replaceThis c =
    (replace shortName "{This}")
    . (replace (name c) "{This}")
    <$> (cardText c)
    where shortName = subRegex (mkRegex ", .*$") (name c) ""
          -- This handles the THS Gods, Tymaret, Jarad, etc.
          -- since only their first names are used in ability text

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
