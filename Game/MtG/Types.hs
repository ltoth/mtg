{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Game.MtG.Types where

import Data.Data
import Data.Function (on)
import Control.Lens
import Data.IntMap (IntMap)
import Data.Set (Set)
import Data.Text (Text)
import Data.Word (Word8)

-- |
-- = Types for card attributes

data Layout = Normal | Split | Flip | DoubleFaced | TokenLayout | Plane
            | Scheme | Phenomenon
              deriving (Show, Eq, Data, Typeable)
type Name = Text

type ManaCost = [ManaSymbol]
data ManaSymbol = W | U | B | R | G | S | CL Word8 | X | Y | Z
                  | GW | WU | RW | WB | UB | GU | UR | BR | BG | RG
                  | W2 | U2 | B2 | R2 | G2 | WP | UP | BP | RP | GP | P
                  deriving (Show, Eq, Data, Typeable)

data ManaType = ManaAnyOneColor | ManaAnyColor | ManaThatColor
              | ManaAnyCombination | ManaAnyCombinationOf [ManaSymbol]
              | ManaSymbols [[ManaSymbol]] -- OrList ([ManaSymbol])
              deriving (Show, Eq, Data, Typeable)

type CMC = Word8

data Color = White | Blue | Black | Red | Green
             deriving (Show, Eq, Data, Typeable)

type TypeLine = Text

data Supertype = Basic | Legendary | Ongoing | Snow | World
                 deriving (Show, Eq, Data, Typeable)

data Type = Instant | Sorcery | Artifact | Creature | Enchantment
            | Land | Planeswalker | Tribal
            deriving (Show, Eq, Data, Typeable)

data Subtype = ArtifactType ArtifactType
             | EnchantmentType EnchantmentType
             | LandType LandType
             | PlaneswalkerType PlaneswalkerType
             | SpellType SpellType
             | CreatureType CreatureType
            deriving (Show, Eq, Data, Typeable)

data ArtifactType = Contraption | Equipment | Fortification
                     deriving (Show, Eq, Data, Typeable)

data EnchantmentType = Aura | Curse | Shrine
                     deriving (Show, Eq, Data, Typeable)

data LandType = BasicLand BasicLandType
              | Desert | Gate | Lair | Locus | Mine | PowerPlant
              | Tower | Urzas
              deriving (Show, Eq, Data, Typeable)

data BasicLandType = Forest | Island | Mountain | Plains | Swamp
                   deriving (Show, Eq, Data, Typeable)

data PlaneswalkerType = Ajani | Ashiok | Bolas | Chandra | Domri | Elspeth
                      | Garruk | Gideon | Jace | Karn | Kiora | Koth | Liliana
                      | Nissa | Ral | Sarkhan | Sorin | Tamiyo | Tezzeret
                      | Tibalt | Venser | Vraska | Xenagos
                      deriving (Show, Eq, Data, Typeable)

data SpellType = Arcane | Trap
               deriving (Show, Eq, Data, Typeable)

data CreatureType = Advisor | Ally | Angel | Anteater | Antelope | Ape
                  | Archer | Archon | Artificer | Assassin
                  | AssemblyWorker | Atog | Aurochs | Avatar | Badger
                  | Barbarian | Basilisk | Bat | Bear | Beast | Beeble
                  | Berserker | Bird | Blinkmoth | Boar | Bringer
                  | Brushwagg | Camarid | Camel | Caribou | Carrier | Cat
                  | Centaur | Cephalid | Chimera | Citizen | Cleric
                  | Cockatrice | Construct | Coward | Crab | Crocodile
                  | Cyclops | Dauthi | Demon | Deserter | Devil | Djinn
                  | Dragon | Drake | Dreadnought | Drone | Druid | Dryad
                  | Dwarf | Efreet | Elder | Eldrazi | Elemental | Elephant
                  | Elf | Elk | Eye | Faerie | Ferret | Fish | Flagbearer
                  | Fox | Frog | Fungus | Gargoyle | Germ | Giant | Gnome
                  | Goat | Goblin | God | Golem | Gorgon | Graveborn
                  | Gremlin | Griffin | Hag | Harpy | Hellion | Hippo
                  | Hippogriff | Homarid | Homunculus | Horror | Horse
                  | Hound | Human | Hydra | Hyena | Illusion | Imp
                  | Incarnation | Insect | Jellyfish | Juggernaut | Kavu
                  | Kirin | Kithkin | Knight | Kobold | Kor | Kraken
                  | Lammasu | Leech | Leviathan | Lhurgoyf | Licid | Lizard
                  | Manticore | Masticore | Mercenary | Merfolk | Metathran
                  | Minion | Minotaur | Monger | Mongoose | Monk | Moonfolk
                  | Mutant | Myr | Mystic | Nautilus | Nephilim | Nightmare
                  | Nightstalker | Ninja | Noggle | Nomad | Nymph | Octopus
                  | Ogre | Ooze | Orb | Orc | Orgg | Ouphe | Ox | Oyster
                  | Pegasus | Pentavite | Pest | Phelddagrif | Phoenix
                  | Pincher | Pirate | Plant | Praetor | Prism | Rabbit
                  | Rat | Rebel | Reflection | Rhino | Rigger | Rogue
                  | Sable | Salamander | Samurai | Sand | Saproling | Satyr
                  | Scarecrow | Scorpion | Scout | Serf | Serpent | Shade
                  | Shaman | Shapeshifter | Sheep | Siren | Skeleton
                  | Slith | Sliver | Slug | Snake | Soldier | Soltari
                  | Spawn | Specter | Spellshaper | Sphinx | Spider | Spike
                  | Spirit | Splinter | Sponge | Squid | Squirrel
                  | Starfish | Surrakar | Survivor | Tetravite | Thalakos
                  | Thopter | Thrull | Treefolk | Triskelavite | Troll
                  | Turtle | Unicorn | Vampire | Vedalken | Viashino
                  | Volver | Wall | Warrior | Weird | Werewolf | Whale
                  | Wizard | Wolf | Wolverine | Wombat | Worm | Wraith
                  | Wurm | Yeti | Zombie | Zubera
                  deriving (Show, Eq, Data, Typeable)

data Rarity = Common | Uncommon | Rare | MythicRare | BasicLandRarity
              deriving (Show, Eq, Data, Typeable)

type RulesText = Text

type Flavor = Text

type Artist = Text

type CardNumber = Text

-- FIXME: Should parse this into our data type using Parsec
type Power = Text

-- FIXME: Should parse this into our data type using Parsec
type Toughness = Text

type Loyalty = Word8

type MultiverseID = Int

type ImageName = Text

type Watermark = Text

data Border = BlackBorder | WhiteBorder | SilverBorder
              deriving (Show, Eq, Data, Typeable)

-- |
-- = Types for parsed abilities

data Cost = CMana ManaCost | CTap | CUntap | CLoyalty NumChange
          | CEffect Effect
          deriving (Show, Eq, Data, Typeable)

data Targets = Target CountRange [TargetMatch]
             | NoTarget (Maybe CountRange) [TargetMatch]
             deriving (Show, Eq, Data, Typeable)

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
                 deriving (Show, Eq, Data, Typeable)

data SpellMatch = SpellMatch ColorMatch [PermanentTypeMatch]
                deriving (Show, Eq, Data, Typeable)

-- TODO: CardMatch should probably also match colors at least
data CardMatch = TopCardsOfLibrary NumValue Zone
               | CardMatch [PermanentTypeMatch] (Maybe Quality) (Maybe Zone)
               deriving (Show, Eq, Data, Typeable)

data Quality = QPower CountRange | QToughness CountRange
             | QCMC CountRange
             deriving (Show, Eq, Data, Typeable)

-- TODO: Ability should be Non Ability ("with" vs. "without")
data PermanentMatch = PermanentMatch (Maybe BlockedStatus)
                        [CombatStatus] ColorMatch
                        NonToken PermanentTypeMatch [Ability] (Maybe Quality)
                        (Maybe Name) (Maybe OwnControl)
                    deriving (Show, Eq, Data, Typeable)

data ColorMatch = CMColors [Non Color] | CMMonocolored | CMMulticolored
                deriving (Show, Eq, Data, Typeable)

data BlockedStatus = Blocked | Unblocked
                   deriving (Show, Eq, Data, Typeable)

data CombatStatus = Attacking | Blocking
                  deriving (Show, Eq, Data, Typeable)

data NonToken = NonToken | CardOrToken
              deriving (Show, Eq, Data, Typeable)

data OwnControl = Own PlayerMatch | Control PlayerMatch
                deriving (Show, Eq, Data, Typeable)

data PermanentTypeMatch = PermanentTypeMatch [Non Supertype] [Non Type]
                            [Non Subtype]
                        | PTMToken | PTMPermanent
                        deriving (Show, Eq, Data, Typeable)

data Non a = Non Bool a
           deriving (Show, Eq, Data, Typeable)

data PermanentStatus =
    PermanentStatus TapStatus FlipStatus FaceStatus PhaseStatus
    deriving (Show, Eq, Data, Typeable)

data PermanentStatusMatch =
    PermanentStatusMatch (Maybe TapStatus) (Maybe FlipStatus) (Maybe FaceStatus) (Maybe PhaseStatus)
    deriving (Show, Eq, Data, Typeable)

data TapStatus = Tapped | Untapped
               deriving (Show, Eq, Data, Typeable)

data FlipStatus = Flipped | Unflipped
                deriving (Show, Eq, Data, Typeable)

data FaceStatus = FaceUp | FaceDown
                deriving (Show, Eq, Data, Typeable)

data PhaseStatus = PhasedIn | PhasedOut
                 deriving (Show, Eq, Data, Typeable)

data CountRange = UpTo Count | Exactly Count | AtLeast Count
                | OneOf [NumValue] | AnyNumber | Other
                deriving (Show, Eq, Data, Typeable)

data Count = AnyCount NumValue | OtherCount NumValue
           deriving (Show, Eq, Data, Typeable)

data NumValue = NumValue Word8 | NumValueX | All | NumVariable Calculation
              | ThatMuch
              deriving (Show, Eq, Data, Typeable)

data NumChange = Plus NumValue | Minus NumValue
               deriving (Show, Eq, Data, Typeable)

-- FIXME: Actually parse calculations properly
type Calculation = Text

-- FIXME: Should this be parsed into possible counter types?
type CounterType = Text

type DurationOrTriggerEvent = Either Duration TriggerEvent

data Duration = DurationUntil TriggerEvent | DurationForAsLongAs TriggerEvent
              | DurationDuring (Maybe PlayerMatch) (Maybe Next) Step
              | DurationEachTurn  -- FIXME: Perhaps "each turn" shouldn't be a duration?
              | DurationEachCombat -- FIXME: Perhaps "each combat" shouldn't either?
              deriving (Show, Eq, Data, Typeable)

-- FIXME: Should be ZoneMatch, and have an enumeration type Zone w/o fields
data Zone = Library Targets | TopOfLibrary Targets
          | BottomOfLibrary Targets | Hand Targets
          | Graveyard Targets
          | Battlefield | Stack | ExileZone | Command | ZoneIt
          deriving (Show, Eq, Data, Typeable)

data TriggerEvent = TEAt (Maybe PlayerMatch) (Maybe Next) Step
                  | TEThisETB | TEThisLTB
                  | TEThisETBOrDies | TEThisDies
                  | TEObjectETB PermanentMatch
                  | TEObjectLTB PermanentMatch
                  | TEOther Text -- FIXME: Make more value constr.
                  deriving (Show, Eq, Data, Typeable)

data Next = Next deriving (Show, Eq, Data, Typeable)

-- TODO: Prefix all constructors with PM?
data PlayerMatch = EachPlayer | You | PMPlayer | Players | Opponent | Opponents
                 | Controller TargetMatch | Owner TargetMatch | HisOrHer | Their
                 | ThatPlayer | ThosePlayers
                 deriving (Show, Eq, Data, Typeable)

data Step = UntapStep | Upkeep | DrawStep | PreCombatMain
          | BeginningOfCombat | DeclareAttackers | DeclareBlockers
          | CombatDamage | EndOfCombat | PostCombatMain
          | End | Cleanup
          deriving (Show, Eq, Data, Typeable)

data Divided = Divided deriving (Show, Eq, Data, Typeable)

data FromAmong = FromAmong deriving (Show, Eq, Data, Typeable)

data CardOrder = AnyOrder | RandomOrder
               deriving (Show, Eq, Data, Typeable)

type TriggerCondition = Text -- TODO: should this be the same as AltCostCondition?
type ActivationInst = Text
type AltCostCondition = Text

data Ability = AdditionalCost ([Cost])
             | AlternativeCost ([Cost]) (Maybe [AltCostCondition])
             | KeywordAbility Keyword
             | ActivatedAbility ([Cost]) [Effect] (Maybe ActivationInst)
             | TriggeredAbility TriggerEvent [Effect] (Maybe [TriggerCondition])
             | StaticAbility [Effect]
             | SpellAbility [Effect]
             deriving (Show, Eq, Data, Typeable)

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
      (Maybe [Ability])
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

    | GetEmblem [Ability]
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
    | OtherEffect Text
    deriving (Show, Eq, Data, Typeable)

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
             deriving (Show, Eq, Data, Typeable)

type SetCode = Text

-- FIXME: Prepend with card and use makeFields instead of makeLenses
data Card = Card
          { _cardLayout :: Layout
          , _cardTypeLine :: TypeLine
          , _cardTypes :: [Type]
          , _cardColors :: [Color]
          , _cardMultiverseID :: MultiverseID
          , _cardName :: Name
          , _cardNames :: [Name]
          , _cardSupertypes :: [Supertype]
          , _cardSubtypes :: [Subtype]
          , _cardCmc :: Maybe CMC
          , _cardRarity :: Rarity
          , _cardArtist :: Artist
          , _cardPower :: Maybe Power
          , _cardToughness :: Maybe Toughness
          , _cardLoyalty :: Maybe Loyalty
          , _cardManaCost :: Maybe ManaCost
          , _cardRulesText :: Maybe RulesText
          , _cardAbilities :: [Ability]
          , _cardCardNumber :: CardNumber
          , _cardVariations :: [MultiverseID]
          , _cardImageName :: ImageName
          , _cardWatermark :: Maybe Watermark
          , _cardCardBorder :: Maybe Border
          , _cardSetCode :: SetCode
          } deriving (Show, Data, Typeable)

makeFields ''Card

-- |
-- = Types for card sets

type SetName = Text

-- TODO: Should be UTCTime or something?
type SetRelease = Text

data SetType = Core | Expansion | Reprint | Box | Un | FromTheVault
               | PremiumDeck | DuelDeck | Starter | Commander
               | Planechase | Archenemy | Promo
              deriving (Show, Eq)

type SetBlock = Text

-- Type for card set as parsed from JSON
-- FIXME: Rename to RawCardSet?
data CardSet' = CardSet'
              { _setName' :: SetName
              , _code' :: SetCode
              , _release' :: SetRelease
              , _border' :: Border
              , _setType' :: SetType
              , _block' :: Maybe SetBlock
              , _cards' :: [Card]
              } deriving (Show)

makeLenses ''CardSet'

-- Type for card set as persisted
data CardSet = CardSet
             { _setName :: SetName
             , _code :: SetCode
             , _release :: SetRelease
             , _border :: Border
             , _setType :: SetType
             , _block :: Maybe SetBlock
             , _cardMultiverseIDs :: [MultiverseID]
             } deriving (Show, Typeable)

makeLenses ''CardSet

-- |
-- = Types for the game engine

data Characteristics = Characteristics
                     { _characteristicsName :: Name
                     , _characteristicsManaCost :: Maybe ManaCost
                     , _characteristicsColors :: [Color]
                     , _characteristicsTypes :: [Type]
                     , _characteristicsSubtypes :: [Subtype]
                     , _characteristicsSupertypes :: [Supertype]
                     , _characteristicsRulesText :: Maybe RulesText
                     , _characteristicsAbilities :: [Ability]
                     , _characteristicsPower :: Maybe Power
                     , _characteristicsToughness :: Maybe Toughness
                     , _characteristicsLoyalty :: Maybe Loyalty
                     } deriving (Show, Data, Typeable)

makeFields ''Characteristics

-- Object ID
type OId = Int

-- Player ID
type PId = Int

data Object a = Object
              { _oid :: OId
              , _owner :: PId
              , _object :: a
              } deriving (Data, Typeable)

makeLenses ''Object

instance Eq (Object a) where
  (==) = (==) `on` (^.oid)

instance Ord (Object a) where
  compare = compare `on` (^.oid)

instance Show OCard where
  show (Object i p c) = "OCard " ++ show i ++ " - " ++ show p ++ " - " ++ show (c^.name)

instance Show OPermanent
instance Show OSpell
instance Show OStackAbility
instance Show OEmblem

type OCard         = Object Card
type OPermanent    = Object Permanent
type OSpell        = Object Spell
type OStackAbility = Object StackAbility
type OEmblem       = Object Emblem
-- TODO: Implement Copy (perhaps only of spells, since permanents could be
-- done within the Permanent type?)  Or is there no reason to have a separate
-- type for copies, even of spells?  Having no separate type would be useful
-- for TargetMatch.
-- type GCopy = Object Copy

-- FIXME: This is totally broken!
data StackObject = OSpell | OStackAbility -- | GCopy FIXME
                 deriving (Show, Data, Typeable)

data Permanent = PCard
               { _pcardCard :: Card
               , _pcardCharacteristics :: Characteristics
               , _pcardController :: PId
               , _pcardPermanentStatus :: PermanentStatus
               , _pcardSummoningSick :: Bool
               , _pcardLoyaltyAlreadyActivated :: Bool
               -- TODO: Add more fields: activatedAbilityAlreadyActivated
               }
               | PToken
               { _ptokenCopyOfCard :: Maybe Card
               , _ptokenCharacteristics :: Characteristics
               , _ptokenController :: PId
               , _ptokenPermanentStatus :: PermanentStatus
               , _ptokenSummoningSick :: Bool
               , _ptokenLoyaltyAlreadyActivated :: Bool
               -- TODO: Add more fields
               }
               deriving (Show, Data, Typeable)

data Spell = Spell
           { _spellCard :: Card
           , _spellCharacteristics :: Characteristics
           , _spellController :: PId
           -- TODO: Add more fields, i.e. modes, targets, value of X,
           -- additional or alternative costs
           } deriving (Show, Data, Typeable)

data StackAbility = StackAbility
                  { _stackabilityEffects :: [Effect]
                  -- Should this be Maybe [Cost], or just empty list for triggered
                  , _stackabilityActivationCost :: Maybe [Cost]
                  , _stackabilityTriggerCondition :: Maybe [TriggerCondition]
                  , _stackabilitySource :: OId
                  -- TODO: Add more fields, i.e. modes, targets, value of X
                  } deriving (Show, Data, Typeable)

data Emblem = Emblem
            { _emblemAbilities :: [Ability]
            , _emblemController :: PId
            } deriving (Show, Data, Typeable)

--data Copy = Copy

makeFields ''Permanent
makeFields ''Spell
makeFields ''StackAbility
makeFields ''Emblem
-- makeFields ''Copy

type LifeTotal = Int
type PoisonTotal = Word8

-- FIXME: This should be a product type with name, etc.
type PlayerInfo = Text

data Player = Player
            { _library :: [OCard]
            , _hand :: Set OCard
            , _graveyard :: [OCard]
            , _life :: LifeTotal
            , _poison :: PoisonTotal
            , _playerInfo :: PlayerInfo
            } deriving (Show, Typeable)

makeLenses ''Player

type Timestamp = Integer
type TurnNumber = Int
type LandCount = Word8

data Relationships = Relationships
                   { _attachedTo :: IntMap OId
                   , _exiledWith :: IntMap (Set OId)
                   -- TODO: Define more relationships, i.e. soulbond, haunt
                   } deriving (Show, Data, Typeable)

makeLenses ''Relationships

data Game = Game
          { _players :: [Player] -- FIXME: Should this be Seq?
          , _battlefield :: Set OPermanent
          , _stack :: [StackObject] -- TODO: Should this be Seq or something else?
          , _exile :: Set OCard
          , _commandZone :: Set OCard
          , _activePlayerId :: PId
          , _playerWithPriorityId :: Maybe PId
          , _timestamp :: Timestamp
          , _turn :: TurnNumber
          , _landCount :: LandCount
          , _step :: Step
          , _relationships :: Relationships
          , _maxOId :: OId
          } deriving (Show, Typeable)

makeLenses ''Game
