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
, Effect(..) -- FIXME: Should this be exported?
, TriggerEvent(..) -- FIXME: Should this be exported?
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
import Data.List.Split (splitOn)
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
manaCostParser = many1 manaSymbolParser

manaSymbolParser :: ParsecT String u Identity ManaSymbol
manaSymbolParser = try (string "{G/W}" >> return GW)
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

data ManaType = ManaAnyOneColor | ManaAnyColor | ManaThatColor
              | ManaAnyCombination | ManaAnyCombinationOf [ManaSymbol]
              | ManaSymbols [[ManaSymbol]] -- OrList ([ManaSymbol])
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

data Cost = CMana ManaCost | CTap | CUntap | CLoyalty NumChange
          | CEffect Effect
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
                 | TMThis | TMEnchantedPermanent | TMEquippedCreature
                 | TMSacrificed PermanentTypeMatch | TMSacrificedCard
                 | TMIt | TMThey | TMTheRest
                 deriving (Show, Eq)

-- TODO: SpellMatch should probably also match colors at least
data SpellMatch = SpellMatch ColorMatch [PermanentTypeMatch]
                deriving (Show, Eq)

-- TODO: CardMatch should probably also match colors at least
data CardMatch = TopCardsOfLibrary NumValue Zone
               | CardMatch [PermanentTypeMatch] (Maybe Quality)
               deriving (Show, Eq)

data Quality = QPower CountRange | QToughness CountRange
             | QCMC CountRange
             deriving (Show, Eq)

-- TODO: Ability should be Non Ability ("with" vs. "without")
data PermanentMatch = PermanentMatch (Maybe BlockedStatus)
                        [CombatStatus] ColorMatch
                        NonToken PermanentTypeMatch [Ability] (Maybe Quality)
                        (Maybe Name) (Maybe OwnControl)
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
              | ThatMuch
              deriving (Show, Eq)

data NumChange = Plus NumValue | Minus NumValue
               deriving (Show, Eq)

-- FIXME: Actually parse calculations properly
type Calculation = String

-- FIXME: Should this be parsed into possible counter types?
type CounterType = String

type DurationOrTriggerEvent = Either Duration TriggerEvent

data Duration = DurationUntil TriggerEvent | DurationForAsLongAs TriggerEvent
              | DurationDuring (Maybe PlayerMatch) (Maybe Next) Step
              | DurationEachTurn  -- FIXME: Perhaps "each turn" shouldn't be a duration?
              | DurationEachCombat -- FIXME: Perhaps "each combat" shouldn't either?
              deriving (Show, Eq)

data Zone = Library Targets | TopOfLibrary Targets
          | BottomOfLibrary Targets | Hand Targets
          | Graveyard Targets
          | Battlefield | Stack | ExileZone | Command
          deriving (Show, Eq)

data TriggerEvent = TEAt (Maybe PlayerMatch) (Maybe Next) Step
                  | TEThisETB | TEThisLTB
                  | TEThisETBOrDies | TEThisDies
                  | TEObjectETB PermanentMatch
                  | TEObjectLTB PermanentMatch
                  | TEOther String -- FIXME: Make more value constr.
                  deriving (Show, Eq)

data Next = Next deriving (Show, Eq)

data PlayerMatch = EachPlayer | You | Player | Players | Opponent | Opponents
                 | Controller TargetMatch | Owner TargetMatch | HisOrHer | Their
                 | ThatPlayer | ThosePlayers
                 deriving (Show, Eq)

data Step = UntapStep | Upkeep | DrawStep | PreCombatMain
          | BeginningOfCombat | DeclareAttackers | DeclareBlockers
          | CombatDamage | EndOfCombat | PostCombatMain
          | End | Cleanup
          deriving (Show, Eq)

data Divided = Divided deriving (Show, Eq)

data FromAmong = FromAmong deriving (Show, Eq)

data CardOrder = AnyOrder | RandomOrder
               deriving (Show, Eq)

type TriggerCondition = String -- TODO: should this be the same as AltCostCondition?
type ActivationInst = String
type AltCostCondition = String

data Ability = AdditionalCost ([Cost])
             | AlternativeCost ([Cost]) (Maybe [AltCostCondition])
             | KeywordAbility Keyword
             | ActivatedAbility ([Cost]) [Effect] (Maybe ActivationInst)
             | TriggeredAbility TriggerEvent [Effect] (Maybe [TriggerCondition])
             | StaticAbility [Effect]
             | SpellAbility [Effect]
             deriving (Show, Eq)

data Effect =
    -- One-shot effects
    Choose Targets Targets (Maybe Zone)
    | Destroy Targets
    | Counter Targets
    | Exile Targets Targets (Maybe FaceStatus) (Maybe DurationOrTriggerEvent)

    -- who, what, from, from among, to, tap status, attached to, under control,
    -- order, trigger event (for delayed zone change)
    | ZoneChange Targets Targets (Maybe Zone) (Maybe FromAmong) Zone
      (Maybe TapStatus) (Maybe Targets) (Maybe OwnControl) (Maybe CardOrder)
      (Maybe TriggerEvent)
    | RevealZone Targets Zone
    | RevealCards Targets Targets (Maybe Zone)
    | Tap Targets
    | Untap Targets
    | LoseLife Targets NumValue
    | GainLife Targets NumValue
    | PayLife NumValue
    | AddAbilities Targets [Ability] (Maybe Duration)
    | ModifyPT Targets NumChange NumChange (Maybe Duration)
    | DealDamage Targets NumValue (Maybe Divided) Targets
    | DrawCard Targets NumValue
    | Sacrifice Targets Targets
    | Discard Targets Targets
    | Regenerate Targets
    | GainControl Targets Targets (Maybe Duration)
    | RemoveCounters CountRange (Maybe CounterType) Targets
    | PutCounters CountRange (Maybe CounterType) Targets
    | PutTokens Targets NumValue NumValue NumValue PermanentMatch
    | AddMana (Maybe CountRange) ManaType

    -- who, which zone, for what
    | SearchZone Targets Zone Targets

    | ShuffleInto Targets Targets Zone
    | Shuffle Targets Zone

    -- what, by, except by, duration
    | CantBeBlocked Targets (Maybe Targets) (Maybe Targets) (Maybe Duration)

    -- what, whom, duration
    | CantBlock Targets (Maybe Targets) (Maybe Duration)

    -- what, whom, duration
    | CanBlockAdditional Targets Targets (Maybe Duration)

    -- what, whom
    | CanBlockOnly Targets Targets

    -- what, by whom (exactly), duration
    | MustBeBlockedIfAble Targets (Maybe Targets) (Maybe Duration)

    -- what, whom, duration
    | AttackIfAble Targets (Maybe Targets) (Maybe Duration)

    | CantBeRegenerated Targets (Maybe Duration)
    | DoesntUntap Targets Duration (Maybe Duration)

    | ETBTapStatus Targets TapStatus
    | ETBWithCounters Targets CountRange (Maybe CounterType)

    | Emblem [Ability]
      -- TODO: PermanentStatusMatch for "tapped"
      -- TODO: CombatStatus for "attacking" "blocking"

    -- Keyword actions
    | Monstrosity NumValue
    | Scry NumValue

    -- TODO: Parse "for each" multipliers, which can
    -- be at the beginning (Curse of the Swine) or end
    -- of the effect (Nemesis of Mortals)

    -- Other effects
    | ModalEffects CountRange [Effect]
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
                      Right xs -> xs
                      -- FIXME: Perhaps we shouldn't flatten the list, so
                      -- that when Artisan's Sorrow has an illegal target,
                      -- we know not to resolve Scry 2. Those effects are
                      -- one ability.
  where paras = concat <$> abilityPara `sepBy` (string "\n\n") <* eof
        abilityPara = try (keyword `sepBy1` commas)
                  <|> (optional abilityWord >>
                       many1 (try additional
                             <|> try alternative
                             <|> try alternativeFree
                             <|> try activated
                             <|> try triggered
                             <|> try basicLand
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
                 <|> try (string "Inspired")
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
            es <- effects
            return $ TriggeredAbility event es cond

        trigEvent =
              try (ciString "At " >> turnEvent)
          <|> try (ciString "Whenever " >> TEOther <$> many (noneOf ",\n"))
          <|> try (ciString "When " >> effectEvent)

        duration = try (optional (string " ")) *>
               (try (DurationUntil <$ ciString "until "
                      <*> (turnEvent <|> effectEvent))
               <|> try (DurationForAsLongAs <$ optional (ciString "for ")
                      <* ciString "as long as " <*> effectEvent)
               <|> try (DurationUntil (TEAt (Just EachPlayer) Nothing Cleanup)
                     <$ ciString "this turn")
               <|> try (DurationDuring <$ ciString "during "
                     <*> optionMaybe playerMatch
                     <*> optionMaybe next
                     <*> step)
               <|> try (DurationEachTurn <$ ciString "each turn")
               <|> try (DurationEachCombat <$ ciString "each combat"))

        -- TODO: first check "at the beginning of combat on your turn"
        -- (Battle-Rattle Shaman)
        -- TODO: first check "at the beginning of each of your main phases"
        -- (Carpet of Flowers)
        turnEvent = try (TEAt <$ ciString "the beginning of "
                      <*> optionMaybe playerMatch
                      <*> optionMaybe next
                      <*> step)
                    <|> try (TEAt (Just EachPlayer) Nothing EndOfCombat
                      <$ optional (string "the ")
                      <* ciString "end of combat")
                    <|> try (TEAt (Just EachPlayer) Nothing Cleanup
                      <$ ciString "end of turn")

        durationOrTrigEvent =
              try (Left <$> duration)
          <|> try (Right <$> trigEvent)

        next = try (Next <$ optional (string "the ") <* string "next ")

        -- TODO: Should really return OrList [TrigEvent]
        -- for cards like Ashen Rider, Absolver Thrull
        -- TODO: Should use permanentMatch to match what
        -- ETBs/dies/LTBs, and consolidate the This value
        -- constructors with the Other
        effectEvent = try (ciString "{This} enters the battlefield or dies" >>
                      return TEThisETBOrDies)
                    <|> try (ciString "{This} enters the battlefield" >>
                      return TEThisETB)
                    <|> try (ciString "{This} dies" >>
                      return TEThisDies)
                    <|> try (ciString "{This} leaves the battlefield" >>
                      return TEThisLTB)
                    <|> TEOther <$> many (noneOf ",\n")

        playerMatch =
          (try (   try (ciString "each player's")
               <|> try (ciString "each player")
            >> return EachPlayer)
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
          <|> try (try (ciString "players'")
               <|> try (ciString "players")
            >> return Players)
          <|> try (try (ciString "player's")
               <|> try (ciString "player")
               <|> try (ciString "any player's") -- FIXME: Perhaps this should be
               -- a separate value constructor, i.e. AnyPlayer
               <|> try (ciString "any player")
            >> return Player)
          <|> try (Controller <$> (it <|> they) <* optional possessive
                <* optional (string " ") <* string "controller" <* optional (string "s")
                <* optional possessive)
          <|> try (Owner <$> (it <|> they) <* optional possessive
                <* optional (string " ") <* string "owner" <* optional (string "s")
                <* optional possessive)
          <|> try (try (ciString "your")
               <|> try (ciString "you")
            >> return You)
          <|> try (ciString "his or her" >> return HisOrHer)
          <|> try (ciString "their" >> return Their)
          <|> try (ciString "that player's" >> return ThatPlayer)
          <|> try (ciString "that player" >> return ThatPlayer)
          <|> try (ciString "those players'" >> return ThosePlayers)
          <|> try (ciString "those players" >> return ThosePlayers)
          <|> try (ciString "each" >>
                     notFollowedBy (ciString " " *> permanentMatch) >>
                     return EachPlayer))
          <* optional (string " ")

        possessive = try (string "'s") <|> try (string "'")

        step =
          (try (ciString "untap step" >> return UntapStep)
          <|> try (ciString "upkeep" >> return Upkeep)
          <|> try (ciString "draw step" >> return DrawStep)
          <|> try (ciString "precombat main phase" >> return PreCombatMain)
          <|> try (ciString "combat" >> return BeginningOfCombat)
          <|> try (ciString "declare attackers step" >> return DeclareAttackers)
          <|> try (ciString "declare blockers step" >> return DeclareBlockers)
          <|> try (ciString "postcombat main phase" >> return PostCombatMain)
          <|> try (ciString "end step" >> return End)
          <|> try (ciString "cleanup step" >> return Cleanup))
          <* optional (string "s")

        manaType =
          try (ManaAnyOneColor <$ ciString "mana of any one color")
          <|> try (ManaAnyColor <$ ciString "mana of any color")
          <|> try (ManaThatColor <$ ciString "mana of that color")
          <|> try (ManaAnyCombination <$ ciString "mana in any combination of colors")
          <|> try (ManaAnyCombinationOf <$>
                    (ciString "mana in any combination of " *>
                    manaSymbolParser `sepBy2` manaCombinationSep))
          <|> try (ManaSymbols <$> manaCostParser `sepBy1` orSep)

        manaCombinationSep = try (string ", and/or ")
                         <|> try (string ", ")
                         <|> try (string " and/or ")

        effects =
          -- Syntax that combines PT modifying and ability gaining
          try (do
                  optional (ciString "each ")
                  ts <- targets
                  pt <- modifyPTPartial
                  ciString " and "
                  ks <- addAbilitiesPartial
                  d <- optionMaybe duration
                  return $ [(uncurry (ModifyPT ts)) pt d,
                            AddAbilities (NoTarget Nothing [TMIt]) ks d])
                  <* optional (numVariableConsume)
                  <* optional (string ".") <* optional (string " ")

          -- Most common case -- several effects, one in each sentence
          <|> many1 effect

        modifyPTPartial = do
          p <- try $ (optional (string " ") >>
                 optional (string "each ") >>
                 ciString "get" >> optional (string "s")
                 >> string " ") *> numChange
          t <- try $ string "/" *> numChange
          return (p, t)

        addAbilitiesPartial =
          try $ (optional (string " ") >>
                optional (string "each ") >>
                try (ciString "gain" <* optional (string "s"))
                  <|> try (ciString "have") <|> (ciString "has")
                >> string " ") *>
                (try (keyword `sepBy1` andSep)
                  <|> quotedAbilities)

        quotedAbilities = string "\"" *> abilityPara <* string "\""

        divided = try (string "divided as you choose among "
                      >> return Divided)

        zone =
          try (ciString "the battlefield" >> return Battlefield)
          <|> try (ciString "the stack" >> return ExileZone)
          <|> try (ciString "the command zone" >> return Command)
          <|> try (TopOfLibrary <$> (optional (try $ string "the ") >>
                  string "top of " >> targets)
                  <* (ciString "library" <|> ciString "libraries"))
          <|> try (BottomOfLibrary <$> (optional (try $ string "the ") >>
                  string "bottom of " >> targets)
                  <* (ciString "library" <|> ciString "libraries"))
          <|> try (Library <$> targets
                  <* (ciString "library" <|> ciString "libraries"))
          <|> try (Hand <$> targets
                  <* ciString "hand" <* optional (string "s"))
          <|> try (Graveyard <$> targets
                  <* ciString "graveyard" <* optional (string "s"))

        faceStatus =
              try (FaceUp <$ string "face up")
          <|> try (FaceDown <$ string "face down")

        fromAmong = try (FromAmong <$ string "from among them")

        cardOrder =
              try (AnyOrder <$ string "in any order")
          <|> try (RandomOrder <$ string "in a random order")

        optionPlayerYou = option (NoTarget Nothing [TMPlayer You]) targets

        modalSep = try (string "; or ")
               <|> try (string "; and/or ")

        effect =
              (optional (try $ ciString "Then " <|> ciString "and ")) *>
              (try (OptionalEffect <$> playerMatch
                         <*> (try $ ciString "may " *> effect))
              <|> try (ModalEffects <$ ciString "Choose " <*> countRange
                         <* string " — " <*> effect `sepBy2` modalSep)
              <|> try (Choose <$> optionPlayerYou
                         <* try (ciString "choose") <* optional (string "s")
                         <* string " " <*> targets
                         <*> optionMaybe (string "from " *> zone))
              <|> try (ciString "destroy" >> optional (string "s")
                         >> string " " >> Destroy <$> targets)
              <|> try (Counter <$ ciString "counter" <* optional (string "s")
                         <* string " " <*> targets)
              <|> try (Exile <$> optionPlayerYou
                         <* try (ciString "exile") <* optional (string "s")
                         <* string " " <*> targets
                         <*> optionMaybe faceStatus
                         <*> optionMaybe durationOrTrigEvent)
              <|> try (ZoneChange <$> optionPlayerYou
                         <*> (try $ ((ciString "return" <|> ciString "put")
                             >> optional (string "s") >> string " ") *> targets)
                         <*> optionMaybe (try $ string "from " >> zone)
                         <*> optionMaybe (try $ fromAmong)
                         <*> (optional (string " ") >>
                             (try (string "on ") <|>
                               (optional (string "on")
                               >> optional (string "in")
                               >> string "to "))
                             >> zone)
                         <*> optionMaybe (try $ string " " *> tapStatus)
                         <*> optionMaybe (try $ string " attached to " *> targets)
                         <*> optionMaybe (try $ string " under " *> ownControl)
                         <*> optionMaybe (try $ string " " *> cardOrder)
                         <*> optionMaybe (try $ optional (string " ") *> trigEvent))
              <|> try (RevealZone <$> optionPlayerYou
                         <* try (ciString "reveal") <* optional (string "s")
                         <* string " " <*> zone)
              <|> try (RevealCards <$> optionPlayerYou
                         <* try (ciString "reveal") <* optional (string "s")
                         <* string " " <*> targets
                         <*> optionMaybe (string "from " *> zone))
              <|> try (ciString "tap " >> Tap <$> targets)
              <|> try (ciString "untap " >> Untap <$> targets)
              <|> try (LoseLife <$> targets
                         <*> (try $ (ciString "lose" >> optional (string "s")
                             >> string " ") *> numberParser
                             <* (optional (string " ") >> string "life")))
              <|> try (GainLife <$> targets
                         <*> (try $ (ciString "gain" >> optional (string "s")
                             >> string " ") *> numberParser
                             <* (optional (string " ") >> string "life")))
              <|> try (PayLife <$> (ciString "Pay " *> numberParser
                             <* (optional (string " ") >> string "life")))
              <|> try (AddAbilities <$> targets
                        <*> addAbilitiesPartial
                        <*> (optionMaybe duration))
              -- Syntax that starts with duration, i.e.
              -- "Until end of turn, up to two target creatures each gain"
              <|> try (do
                      d <- duration
                      string ", "
                      ts <- targets
                      ks <- addAbilitiesPartial
                      return $ AddAbilities ts ks (Just d))
              <|> try ((uncurry <$> (ModifyPT <$> targets))
                       <*> modifyPTPartial
                       <*> (optionMaybe duration))
              <|> try (DealDamage <$> targets
                         <*> (try $ (ciString "deal" >> optional (string "s")
                             >> string " ") *> numberParser
                             <* (optional (string " ") >> string "damage "))
                         <*> (optionMaybe divided <* optional (string "to "))
                         <*> targets)
              <|> try (DrawCard <$> optionPlayerYou
                         <*> (try $ (ciString "draw" >> optional (string "s")
                             >> string " ") *> numberParser
                             <* (optional (string " ") >> string "card"
                             >> optional (string "s"))))
              <|> try (Sacrifice <$> optionPlayerYou
                         <*> (try $ (ciString "sacrifice" >> optional (string "s")
                             >> string " ") *> targets))
              <|> try (Discard <$> optionPlayerYou
                         <*> (try $ (ciString "discard" >> optional (string "s")
                             >> string " ") *> targets))
              <|> try (Regenerate <$ ciString "regenerate " <*> targets)
              <|> try (GainControl <$> optionPlayerYou
                         <*> (try $ (ciString "gain" >> optional (string "s")
                             >> string " control of ") *> targets)
                         <*> optionMaybe duration)
              <|> try (ciString "Remove " >> RemoveCounters <$> countRange
                         <*> (try $ (optional (string " ") *>
                         optionMaybe (many1 (noneOf " \n")) <* optional (string " "))
                         <* ciString "counter" <* optional (string "s")
                         <* ciString " from ") <*> targets)
              <|> try (ciString "Put " >> PutCounters <$> countRange
                         <*> (try $ (optional (string " ") *>
                         optionMaybe (many1 (noneOf " \n")) <* optional (string " "))
                         <* ciString "counter" <* optional (string "s")
                         <* ciString " on ") <*> targets)
              <|> try (PutTokens <$> optionPlayerYou
                         <*> ((ciString "put" >> optional (string "s")
                             >> string " ") *> numberParser)
                         <*> (string " " *> explicitNumber)
                         <*> (string "/" *> explicitNumber)
                         <*> (string " " *> permanentMatch)
                         <* optional (string " ") <* string "onto the battlefield")
              <|> try (AddMana <$> (ciString "Add to your mana pool " *> optionMaybe countRange)
                         <*> (optional (string " ") *> manaType))
              <|> try (AddMana <$> (ciString "Add " *> optionMaybe countRange)
                         <*> (optional (string " ") *> manaType)
                         <* string " to your mana pool")
              <|> try (SearchZone <$> optionPlayerYou
                         <*> ((ciString "search" >> optional (string "es")
                             >> string " ") *> zone)
                         <*> (string " for " *> targets))
              <|> try (ShuffleInto <$> optionPlayerYou
                         <*> ((ciString "shuffle" >> optional (string "s")
                             >> string " ") *> targets)
                         <*> (string "into " *> zone))
              <|> try (Shuffle <$> optionPlayerYou
                         <*> ((ciString "shuffle" >> optional (string "s")
                             >> string " ") *> zone))
              <|> try (CantBeBlocked <$> (targets <* ciString "can't be blocked")
                         <*> optionMaybe (ciString " by " *> targets)
                         <*> optionMaybe (ciString " except by " *> targets)
                         <*> optionMaybe (optional (string " ") *> duration))
              <|> try (CantBlock <$> (targets <* ciString "can't block")
                         <*> optionMaybe (try $ string " " *> targets)
                         <*> optionMaybe (optional (string " ") *> duration))
              <|> try (CanBlockAdditional <$> (targets <* ciString "can block an additional ")
                         <*> targets
                         <*> optionMaybe (optional (string " ") *> duration))
              <|> try (CanBlockOnly <$> (targets <* ciString "can block only ")
                         <*> targets)
              <|> try (MustBeBlockedIfAble <$> targets <* ciString "must be blocked"
                         <*> optionMaybe (try $ string " by exactly " *> targets)
                         <*> optionMaybe duration
                         <* ciString " if able")
              <|> try (AttackIfAble <$> (targets <* ciString "attack"
                             <* optional (string "s"))
                         <*> optionMaybe (try $ string " " *> targets)
                         <*> optionMaybe (optional (string " ") *> duration)
                         <* ciString " if able")
              <|> try (CantBeRegenerated <$> (targets <* ciString "can't be regenerated")
                         <*> optionMaybe duration)
              <|> try (DoesntUntap <$> targets
                         <* (ciString "doesn't" <|> ciString "don't")
                         <* ciString " untap" <*> duration
                         <*> optionMaybe duration)
              <|> try (ETBTapStatus <$> (targets <* ciString "enters the battlefield ")
                         <*> tapStatus)
              <|> try (ETBWithCounters <$> (targets <* ciString "enters the battlefield with ")
                         <*> countRange
                         <*> (try $ (optional (string " ") *>
                         optionMaybe (many1 (noneOf " \n")) <* optional (string " "))
                         <* ciString "counter" <* optional (string "s")
                         <* ciString " on it"))
              <|> try (Emblem <$ ciString "You get an emblem with "
                         <*> quotedAbilities)
              <|> try (Monstrosity <$ ciString "Monstrosity "
                         <*> explicitNumber)
              <|> try (Scry <$ ciString "Scry " <*> explicitNumber)
              <|> (OtherEffect <$> many1 (noneOf ".\n\""))
              ) <* optional (numVariableConsume)
              <* optional (oneOf ".,") <* optional (string " ")

        basicLand =
          try (ActivatedAbility [CTap] [AddMana Nothing (ManaSymbols [[W]])] Nothing
              <$ string "W" <* eof)
          <|> try (ActivatedAbility [CTap] [AddMana Nothing (ManaSymbols [[U]])] Nothing
              <$ string "U" <* eof)
          <|> try (ActivatedAbility [CTap] [AddMana Nothing (ManaSymbols [[B]])] Nothing
              <$ string "B" <* eof)
          <|> try (ActivatedAbility [CTap] [AddMana Nothing (ManaSymbols [[R]])] Nothing
              <$ string "R" <* eof)
          <|> try (ActivatedAbility [CTap] [AddMana Nothing (ManaSymbols [[G]])] Nothing
              <$ string "G" <* eof)

        activated = ActivatedAbility <$> totalCost
          <*> (string ": " *> effects)
          <*> optionMaybe activationInst

        -- FIXME: This will only work once all effects are accounted for
        -- and we take out OtherEffect
        activationInst = try $ string "Activate this ability only "
                               *> many1 (noneOf ("\n"))

        totalCost = abilityCost `sepBy1` andSep
        andSep = try (string ", and ")
                     <|> try (string ", ")
                     <|> try (string " and ")
                     <|> string ","  -- FIXME: Is this necessary?
        abilityCost = try (string "{T}" >> return CTap)
                  <|> try (string "{Q}" >> return CUntap)
                  <|> try ((optional (ciString "Pay ") >>
                        CMana <$> manaCostParser))
                  <|> try (CLoyalty <$> numChange)
                  <|> try (string "0" >> (return $ CLoyalty (Plus (NumValue 0))))
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

        numberParser = try (All <$ (ciString "all" <|> ciString "each"))
                  <|> try (ThatMuch <$ ciString "that "
                          <* (try (string "much") <|> try (string "many")))
                  <|> try (do
                          optional $ try (string "a number of")
                          optional $ try (string "an amount of")
                          lookAhead $ try (do
                            noneOf (",.\n") `manyTill` try (string "equal to ")
                            NumVariable <$> many1 (noneOf (",.\n"))))
                  <|> try explicitNumber

        explicitNumber = try (do
                           try (string "X")
                           lookAhead $ try (do
                             noneOf (".\n") `manyTill` try (string ", where X is ")
                             NumVariable <$> many1 (noneOf (".\n"))))
                  <|> try (string "X" >> (return NumValueX))
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
                  <|> try ((NumValue . read) <$> (many1 digit))

        -- used to consume this input, normally seen using lookAhead
        numVariableConsume =
              try (optional (string " ") *> string "equal to " >> many1 (noneOf (".\n")))
          <|> try (string ", where X is " >> many1 (noneOf (".\n")))

        numChange = try (Plus <$> (string "+" *> explicitNumber))
                <|> try (Minus <$> (string "-" *> explicitNumber))

        -- FIXME: We should distinguish between "or" and "and" here
        -- Artisan's Sorrow, Swan Song, Corrupted Roots etc., Hero's
        -- Downfall
        -- FIXME: Provide more specific target parsers, which will
        -- restrict the types, i.e. exclude players. This could be done
        -- by passing the parser as an parameter to the targets function,
        -- i.e. replacing targetMatch with permanentOrCardTargetMatch
        -- or targetPlayerMatch or cardTargetMatch or permanentTargetMatch
        -- or permanentOrPlayerMatch (damage)
        --
        -- This will allow us to parse the triggered ability of Underworld
        -- Cerberus to distinguish between
        --   (exile it and each player) (returns all creature cards...)
        -- and
        --   (exile it) and (each player returns all creature cards...)
        -- by specifying that exile can only target permanentsOrCards
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
        -- in permanentTypeMatch better
        andOrSep' = try (string ", and/or ")
               <|> try ((string ", and ") <* notFollowedBy effect)
               <|> try (string ", or ")
               <|> try ((string ", ") <* notFollowedBy effect)
               <|> try (string "and/or ")
               <|> try (string "and ")
               <|> try (string "or ")

        targetMatch = try (TMPlayer <$> playerMatch)
                  <|> try it
                  <|> try they
                  <|> try enchanted
                  <|> try equipped
                  <|> try sacrificed
                  <|> try this
                  <|> try theRest
                  <|> try (TMCard <$> cardMatch)
                  <|> try (TMSpell <$> spellMatch)
                  <|> try (TMPermanent <$> permanentMatch)

        -- FIXME: Make this more robust
        it = (try (ciString "that card")
          <|> try (ciString "that " <* permanentTypeMatch)
          <|> try (ciString "its")
          <|> try (ciString "it")
          >> return TMIt)
          <* optional (string " ")  -- to match permanentType's behavior
          -- of consuming trailing spaces

        -- FIXME: Make this more robust
        -- FIXME: Should this be the same as TMIt?
        they = (try (ciString "those cards")
          <|> try (ciString "those " <* permanentTypeMatch)
          <|> try (ciString "they")
          <|> try (ciString "their")
          <|> try (ciString "them")
          >> return TMThey)
          <* optional (string " ")  -- to match permanentType's behavior
          -- of consuming trailing spaces

        enchanted = try (ciString "enchanted " <* permanentTypeMatch)
          >> return TMEnchantedPermanent

        sacrificed =
          optional (try (string "the ")) *>
          (try (TMSacrificed <$ ciString "sacrificed " <*> permanentTypeMatch)
          <|> try (TMSacrificedCard <$ ciString "sacrificed card"))
          <* optional (string " ")  -- to match permanentType's behavior

        equipped = try (TMEquippedCreature <$ ciString "equipped creature")
          <* optional (string " ")  -- to match permanentType's behavior

        this = (try (ciString "this card")
          <|> try (ciString "this " <* permanentTypeMatch)
          <|> try (ciString "{This}")
          >> return TMThis)
          <* optional (string " ")  -- to match permanentType's behavior

        theRest = (try (TMTheRest <$ ciString "the rest"))
          <* optional (string " ")  -- to match permanentType's behavior

        cardMatch =
          (try (TopCardsOfLibrary <$ ciString "the top "
                 <*> option (NumValue 1) explicitNumber
                 <* optional (string " ") <* string "card"
                 <* optional (string "s") <* string " of " <*> zone)
          <|> try (CardMatch <$> (permanentTypeMatch `sepBy` orSep')
                 <* string "card" <* optional (string "s")
                 <*> optionMaybe withQuality))
                 -- TODO: optionMaybe ("in" *> zone)
          <* optional (string " ")  -- to match permanentType's behavior

        spellMatch =
          (try (SpellMatch <$> colorMatch
                  <*> (permanentTypeMatch `sepBy` orSep')
                  <* string "spell" <* optional (string "s")))
          <* optional (string " ")  -- to match permanentType's behavior

        -- FIXME: Remove this and deal with consuming trailing spaces
        -- in permanentTypeMatch better
        orSep' = try (string ", or ")
            <|> try (string ", ")
            <|> try (string "or ")

        permanentMatch = try (do
          -- These are necessary for "a creature, a land, and a Wall"
          optional (try $ string "an ")
          optional (try $ string "a ")

          b <- optionMaybe $ try blocked
          combat <- combatStatuses
          cs <- colorMatch
          nt <- nonToken
          t <- permanentTypeMatch
          as <- withAbilities
          q <- optionMaybe withQuality
          -- TODO: support "with(out) a fate counter on it"
          -- Oblivion Stone
          cardName <- optionMaybe cardNamed
          oc <- optionMaybe ownControl
          -- TODO: support other conditions like
          -- "that dealt damage to you this turn"
          -- Spear of Heliod
          return $ PermanentMatch b combat cs nt t as q cardName oc)

        blocked = try (ciString "blocked " >> return Blocked)
              <|> try (ciString "unblocked " >> return Unblocked)

        tapStatus = try (Tapped <$ ciString "tapped")
                <|> try (Untapped <$ ciString "untapped")

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

        withQuality = try (optional (string " ") *> try (ciString "with ") *>
              (QPower <$> (ciString "power " *> countRange))
          <|> (QToughness <$> (ciString "toughness " *> countRange))
          <|> (QCMC <$> (ciString "converted mana cost " *> countRange)))

        ownControl = (try (do
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
                 ) <* optional (string " ")

        nonParser = option True (try (do
                                string "non"
                                optional (string "-")
                                return False))

        cardNamed = try (do
          optional (string " ")
          string "named "
          many1 (noneOf ",:;.\n")) -- FIXME: Actually match against
          -- possible card names, not just as strings, since
          -- this doesn't know when to stop properly, i.e. Kher Keep

        permanentTypeMatch =
              try (string "permanent" >> optional (string "s") >>
                (return $ Permanent))
          <|> try (string "token" >> optional (string "s") >>
                (return $ Token))
          <|> try (do
                super <- ((try (Non <$> nonParser <*> supertypeParser <* optional (string "s")))
                        `sepEndBy` (string " "))
                sub <- ((try (Non <$> nonParser <*> subtypeParser <* optional (string "s")))
                      `sepEndBy` (string " "))
                t <- ((try (Non <$> nonParser <*> typeParser <* optional (string "s")))
                    `sepEndBy` (string " "))
                optional (try (optional (string " ") *> string "permanent" <* optional (string "s")))
                optional (try (optional (string " ") *> string "token" <* optional (string "s")))
                optional (string " ")
                when (super == [] && sub == [] && t == [])
                  (fail "Did not match any permanent type")
                return $ PermanentTypeMatch super t sub)

        spell = SpellAbility <$> effects

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
             | Protection (Either Quality PlayerMatch) -- FIXME: Color, etc.
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
