{-# LANGUAGE OverloadedStrings #-}

module MagicCards.Subtype
( Subtype(..)
, ArtifactType(..)
, EnchantmentType(..)
, LandType(..)
, BasicLandType(..)
, PlaneswalkerType(..)
, SpellType(..)
, CreatureType(..)
, subtypeParser
) where

import Control.Applicative
import Data.Aeson (FromJSON, parseJSON, Value(..))
import Data.Functor.Identity (Identity)
import qualified Data.Text as T
import Text.Parsec.Char.Extra(ciString)
import Text.Parsec.Prim (ParsecT)
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

data Subtype = ArtifactType ArtifactType
             | EnchantmentType EnchantmentType
             | LandType LandType
             | PlaneswalkerType PlaneswalkerType
             | SpellType SpellType
             | CreatureType CreatureType
            deriving (Show, Eq)
instance FromJSON Subtype where
    parseJSON (String s) = return . stringToSubtype $ T.unpack s
    parseJSON _ = fail "Could not parse subtype"

subtypeParser :: ParsecT String u Identity Subtype
subtypeParser =
      -- Artifact types
      try (ciString "Contraption" >> (return $ ArtifactType Contraption))
  <|> try (ciString "Equipment" >> (return $ ArtifactType Equipment))
  <|> try (ciString "Fortification" >> (return $ ArtifactType Fortification))

      -- Enchantment types
  <|> try (ciString "Aura" >> (return $ EnchantmentType Aura))
  <|> try (ciString "Curse" >> (return $ EnchantmentType Curse))
  <|> try (ciString "Shrine" >> (return $ EnchantmentType Shrine))

      -- Basic land types
  <|> try (ciString "Forest" >> (return $ LandType $ BasicLand Forest))
  <|> try (ciString "Island" >> (return $ LandType $ BasicLand Island))
  <|> try (ciString "Mountain" >> (return $ LandType $ BasicLand Mountain))
  <|> try (ciString "Plains" >> (return $ LandType $ BasicLand Plains))
  <|> try (ciString "Swamp" >> (return $ LandType $ BasicLand Swamp))

      -- Other land types
  <|> try (ciString "Desert" >> (return $ LandType Desert))
  <|> try (ciString "Gate" >> (return $ LandType Gate))
  <|> try (ciString "Lair" >> (return $ LandType Lair))
  <|> try (ciString "Locus" >> (return $ LandType Locus))
  <|> try (ciString "Mine" >> (return $ LandType Mine))
  <|> try (ciString "Power-Plant" >> (return $ LandType PowerPlant))
  <|> try (ciString "Tower" >> (return $ LandType Tower))
  <|> try (ciString "Urza's" >> (return $ LandType Urzas))

      -- Planeswalker types
  <|> try (ciString "Ajani" >> (return $ PlaneswalkerType Ajani))
  <|> try (ciString "Ashiok" >> (return $ PlaneswalkerType Ashiok))
  <|> try (ciString "Bolas" >> (return $ PlaneswalkerType Bolas))
  <|> try (ciString "Chandra" >> (return $ PlaneswalkerType Chandra))
  <|> try (ciString "Domri" >> (return $ PlaneswalkerType Domri))
  <|> try (ciString "Elspeth" >> (return $ PlaneswalkerType Elspeth))
  <|> try (ciString "Garruk" >> (return $ PlaneswalkerType Garruk))
  <|> try (ciString "Gideon" >> (return $ PlaneswalkerType Gideon))
  <|> try (ciString "Jace" >> (return $ PlaneswalkerType Jace))
  <|> try (ciString "Karn" >> (return $ PlaneswalkerType Karn))
  <|> try (ciString "Koth" >> (return $ PlaneswalkerType Koth))
  <|> try (ciString "Liliana" >> (return $ PlaneswalkerType Liliana))
  <|> try (ciString "Nissa" >> (return $ PlaneswalkerType Nissa))
  <|> try (ciString "Ral" >> (return $ PlaneswalkerType Ral))
  <|> try (ciString "Sarkhan" >> (return $ PlaneswalkerType Sarkhan))
  <|> try (ciString "Sorin" >> (return $ PlaneswalkerType Sorin))
  <|> try (ciString "Tamiyo" >> (return $ PlaneswalkerType Tamiyo))
  <|> try (ciString "Tezzeret" >> (return $ PlaneswalkerType Tezzeret))
  <|> try (ciString "Tibalt" >> (return $ PlaneswalkerType Tibalt))
  <|> try (ciString "Venser" >> (return $ PlaneswalkerType Venser))
  <|> try (ciString "Vraska" >> (return $ PlaneswalkerType Vraska))
  <|> try (ciString "Xenagos" >> (return $ PlaneswalkerType Xenagos))

      -- Spell types
  <|> try (ciString "Arcane" >> (return $ SpellType Arcane))
  <|> try (ciString "Trap" >> (return $ SpellType Trap))

      -- Creature types
  <|> try (ciString "Advisor" >> (return $ CreatureType Advisor))
  <|> try (ciString "Ally" >> (return $ CreatureType Ally))
  <|> try (ciString "Allies" >> (return $ CreatureType Ally))
  <|> try (ciString "Angel" >> (return $ CreatureType Angel))
  <|> try (ciString "Anteater" >> (return $ CreatureType Anteater))
  <|> try (ciString "Antelope" >> (return $ CreatureType Antelope))
  <|> try (ciString "Ape" >> (return $ CreatureType Ape))
  <|> try (ciString "Archer" >> (return $ CreatureType Archer))
  <|> try (ciString "Archon" >> (return $ CreatureType Archon))
  <|> try (ciString "Artificer" >> (return $ CreatureType Artificer))
  <|> try (ciString "Assassin" >> (return $ CreatureType Assassin))
  <|> try (ciString "Assembly-Worker" >> (return $ CreatureType AssemblyWorker))
  <|> try (ciString "Atog" >> (return $ CreatureType Atog))
  <|> try (ciString "Aurochs" >> (return $ CreatureType Aurochs))
  <|> try (ciString "Avatar" >> (return $ CreatureType Avatar))
  <|> try (ciString "Badger" >> (return $ CreatureType Badger))
  <|> try (ciString "Barbarian" >> (return $ CreatureType Barbarian))
  <|> try (ciString "Basilisk" >> (return $ CreatureType Basilisk))
  <|> try (ciString "Bat" >> (return $ CreatureType Bat))
  <|> try (ciString "Bear" >> (return $ CreatureType Bear))
  <|> try (ciString "Beast" >> (return $ CreatureType Beast))
  <|> try (ciString "Beeble" >> (return $ CreatureType Beeble))
  <|> try (ciString "Berserker" >> (return $ CreatureType Berserker))
  <|> try (ciString "Bird" >> (return $ CreatureType Bird))
  <|> try (ciString "Blinkmoth" >> (return $ CreatureType Blinkmoth))
  <|> try (ciString "Boar" >> (return $ CreatureType Boar))
  <|> try (ciString "Bringer" >> (return $ CreatureType Bringer))
  <|> try (ciString "Brushwagg" >> (return $ CreatureType Brushwagg))
  <|> try (ciString "Camarid" >> (return $ CreatureType Camarid))
  <|> try (ciString "Camel" >> (return $ CreatureType Camel))
  <|> try (ciString "Caribou" >> (return $ CreatureType Caribou))
  <|> try (ciString "Carrier" >> (return $ CreatureType Carrier))
  <|> try (ciString "Cat" >> (return $ CreatureType Cat))
  <|> try (ciString "Centaur" >> (return $ CreatureType Centaur))
  <|> try (ciString "Cephalid" >> (return $ CreatureType Cephalid))
  <|> try (ciString "Chimera" >> (return $ CreatureType Chimera))
  <|> try (ciString "Citizen" >> (return $ CreatureType Citizen))
  <|> try (ciString "Cleric" >> (return $ CreatureType Cleric))
  <|> try (ciString "Cockatrice" >> (return $ CreatureType Cockatrice))
  <|> try (ciString "Construct" >> (return $ CreatureType Construct))
  <|> try (ciString "Coward" >> (return $ CreatureType Coward))
  <|> try (ciString "Crab" >> (return $ CreatureType Crab))
  <|> try (ciString "Crocodile" >> (return $ CreatureType Crocodile))
  <|> try (ciString "Cyclops" >> (return $ CreatureType Cyclops))
  <|> try (ciString "Dauthi" >> (return $ CreatureType Dauthi))
  <|> try (ciString "Demon" >> (return $ CreatureType Demon))
  <|> try (ciString "Deserter" >> (return $ CreatureType Deserter))
  <|> try (ciString "Devil" >> (return $ CreatureType Devil))
  <|> try (ciString "Djinn" >> (return $ CreatureType Djinn))
  <|> try (ciString "Dragon" >> (return $ CreatureType Dragon))
  <|> try (ciString "Drake" >> (return $ CreatureType Drake))
  <|> try (ciString "Dreadnought" >> (return $ CreatureType Dreadnought))
  <|> try (ciString "Drone" >> (return $ CreatureType Drone))
  <|> try (ciString "Druid" >> (return $ CreatureType Druid))
  <|> try (ciString "Dryad" >> (return $ CreatureType Dryad))
  <|> try (ciString "Dwarf" >> (return $ CreatureType Dwarf))
  <|> try (ciString "Efreet" >> (return $ CreatureType Efreet))
  <|> try (ciString "Elder" >> (return $ CreatureType Elder))
  <|> try (ciString "Eldrazi" >> (return $ CreatureType Eldrazi))
  <|> try (ciString "Elemental" >> (return $ CreatureType Elemental))
  <|> try (ciString "Elephant" >> (return $ CreatureType Elephant))
  <|> try (ciString "Elf" >> (return $ CreatureType Elf))
  <|> try (ciString "Elves" >> (return $ CreatureType Elf))
  <|> try (ciString "Elk" >> (return $ CreatureType Elk))
  <|> try (ciString "Eye" >> (return $ CreatureType Eye))
  <|> try (ciString "Faerie" >> (return $ CreatureType Faerie))
  <|> try (ciString "Ferret" >> (return $ CreatureType Ferret))
  <|> try (ciString "Fish" >> (return $ CreatureType Fish))
  <|> try (ciString "Flagbearer" >> (return $ CreatureType Flagbearer))
  <|> try (ciString "Fox" >> (return $ CreatureType Fox))
  <|> try (ciString "Frog" >> (return $ CreatureType Frog))
  <|> try (ciString "Fungus" >> (return $ CreatureType Fungus))
  <|> try (ciString "Gargoyle" >> (return $ CreatureType Gargoyle))
  <|> try (ciString "Germ" >> (return $ CreatureType Germ))
  <|> try (ciString "Giant" >> (return $ CreatureType Giant))
  <|> try (ciString "Gnome" >> (return $ CreatureType Gnome))
  <|> try (ciString "Goat" >> (return $ CreatureType Goat))
  <|> try (ciString "Goblin" >> (return $ CreatureType Goblin))
  <|> try (ciString "God" >> (return $ CreatureType God))
  <|> try (ciString "Golem" >> (return $ CreatureType Golem))
  <|> try (ciString "Gorgon" >> (return $ CreatureType Gorgon))
  <|> try (ciString "Graveborn" >> (return $ CreatureType Graveborn))
  <|> try (ciString "Gremlin" >> (return $ CreatureType Gremlin))
  <|> try (ciString "Griffin" >> (return $ CreatureType Griffin))
  <|> try (ciString "Hag" >> (return $ CreatureType Hag))
  <|> try (ciString "Harpy" >> (return $ CreatureType Harpy))
  <|> try (ciString "Harpies" >> (return $ CreatureType Harpy))
  <|> try (ciString "Hellion" >> (return $ CreatureType Hellion))
  <|> try (ciString "Hippo" >> (return $ CreatureType Hippo))
  <|> try (ciString "Hippogriff" >> (return $ CreatureType Hippogriff))
  <|> try (ciString "Homarid" >> (return $ CreatureType Homarid))
  <|> try (ciString "Homunculus" >> (return $ CreatureType Homunculus))
  <|> try (ciString "Horror" >> (return $ CreatureType Horror))
  <|> try (ciString "Horse" >> (return $ CreatureType Horse))
  <|> try (ciString "Hound" >> (return $ CreatureType Hound))
  <|> try (ciString "Human" >> (return $ CreatureType Human))
  <|> try (ciString "Hydra" >> (return $ CreatureType Hydra))
  <|> try (ciString "Hyena" >> (return $ CreatureType Hyena))
  <|> try (ciString "Illusion" >> (return $ CreatureType Illusion))
  <|> try (ciString "Imp" >> (return $ CreatureType Imp))
  <|> try (ciString "Incarnation" >> (return $ CreatureType Incarnation))
  <|> try (ciString "Insect" >> (return $ CreatureType Insect))
  <|> try (ciString "Jellyfish" >> (return $ CreatureType Jellyfish))
  <|> try (ciString "Juggernaut" >> (return $ CreatureType Juggernaut))
  <|> try (ciString "Kavu" >> (return $ CreatureType Kavu))
  <|> try (ciString "Kirin" >> (return $ CreatureType Kirin))
  <|> try (ciString "Kithkin" >> (return $ CreatureType Kithkin))
  <|> try (ciString "Knight" >> (return $ CreatureType Knight))
  <|> try (ciString "Kobold" >> (return $ CreatureType Kobold))
  <|> try (ciString "Kor" >> (return $ CreatureType Kor))
  <|> try (ciString "Kraken" >> (return $ CreatureType Kraken))
  <|> try (ciString "Lammasu" >> (return $ CreatureType Lammasu))
  <|> try (ciString "Leech" >> (return $ CreatureType Leech))
  <|> try (ciString "Leeches" >> (return $ CreatureType Leech))
  <|> try (ciString "Leviathan" >> (return $ CreatureType Leviathan))
  <|> try (ciString "Lhurgoyf" >> (return $ CreatureType Lhurgoyf))
  <|> try (ciString "Licid" >> (return $ CreatureType Licid))
  <|> try (ciString "Lizard" >> (return $ CreatureType Lizard))
  <|> try (ciString "Manticore" >> (return $ CreatureType Manticore))
  <|> try (ciString "Masticore" >> (return $ CreatureType Masticore))
  <|> try (ciString "Mercenary" >> (return $ CreatureType Mercenary))
  <|> try (ciString "Mercenaries" >> (return $ CreatureType Mercenary))
  <|> try (ciString "Merfolk" >> (return $ CreatureType Merfolk))
  <|> try (ciString "Metathran" >> (return $ CreatureType Metathran))
  <|> try (ciString "Minion" >> (return $ CreatureType Minion))
  <|> try (ciString "Minotaur" >> (return $ CreatureType Minotaur))
  <|> try (ciString "Monger" >> (return $ CreatureType Monger))
  <|> try (ciString "Mongoose" >> (return $ CreatureType Mongoose))
  <|> try (ciString "Monk" >> (return $ CreatureType Monk))
  <|> try (ciString "Moonfolk" >> (return $ CreatureType Moonfolk))
  <|> try (ciString "Mutant" >> (return $ CreatureType Mutant))
  <|> try (ciString "Myr" >> (return $ CreatureType Myr))
  <|> try (ciString "Mystic" >> (return $ CreatureType Mystic))
  <|> try (ciString "Nautilus" >> (return $ CreatureType Nautilus))
  <|> try (ciString "Nephilim" >> (return $ CreatureType Nephilim))
  <|> try (ciString "Nightmare" >> (return $ CreatureType Nightmare))
  <|> try (ciString "Nightstalker" >> (return $ CreatureType Nightstalker))
  <|> try (ciString "Ninja" >> (return $ CreatureType Ninja))
  <|> try (ciString "Noggle" >> (return $ CreatureType Noggle))
  <|> try (ciString "Nomad" >> (return $ CreatureType Nomad))
  <|> try (ciString "Nymph" >> (return $ CreatureType Nymph))
  <|> try (ciString "Octopus" >> (return $ CreatureType Octopus))
  <|> try (ciString "Octopuses" >> (return $ CreatureType Octopus))
  <|> try (ciString "Ogre" >> (return $ CreatureType Ogre))
  <|> try (ciString "Ooze" >> (return $ CreatureType Ooze))
  <|> try (ciString "Orb" >> (return $ CreatureType Orb))
  <|> try (ciString "Orc" >> (return $ CreatureType Orc))
  <|> try (ciString "Orgg" >> (return $ CreatureType Orgg))
  <|> try (ciString "Ouphe" >> (return $ CreatureType Ouphe))
  <|> try (ciString "Ox" >> (return $ CreatureType Ox))
  <|> try (ciString "Oxes" >> (return $ CreatureType Ox))
  <|> try (ciString "Oyster" >> (return $ CreatureType Oyster))
  <|> try (ciString "Pegasus" >> (return $ CreatureType Pegasus))
  <|> try (ciString "Pentavite" >> (return $ CreatureType Pentavite))
  <|> try (ciString "Pest" >> (return $ CreatureType Pest))
  <|> try (ciString "Phelddagrif" >> (return $ CreatureType Phelddagrif))
  <|> try (ciString "Phoenix" >> (return $ CreatureType Phoenix))
  <|> try (ciString "Pincher" >> (return $ CreatureType Pincher))
  <|> try (ciString "Pirate" >> (return $ CreatureType Pirate))
  <|> try (ciString "Plant" >> (return $ CreatureType Plant))
  <|> try (ciString "Praetor" >> (return $ CreatureType Praetor))
  <|> try (ciString "Prism" >> (return $ CreatureType Prism))
  <|> try (ciString "Rabbit" >> (return $ CreatureType Rabbit))
  <|> try (ciString "Rat" >> (return $ CreatureType Rat))
  <|> try (ciString "Rebel" >> (return $ CreatureType Rebel))
  <|> try (ciString "Reflection" >> (return $ CreatureType Reflection))
  <|> try (ciString "Rhino" >> (return $ CreatureType Rhino))
  <|> try (ciString "Rigger" >> (return $ CreatureType Rigger))
  <|> try (ciString "Rogue" >> (return $ CreatureType Rogue))
  <|> try (ciString "Sable" >> (return $ CreatureType Sable))
  <|> try (ciString "Salamander" >> (return $ CreatureType Salamander))
  <|> try (ciString "Samurai" >> (return $ CreatureType Samurai))
  <|> try (ciString "Sand" >> (return $ CreatureType Sand))
  <|> try (ciString "Saproling" >> (return $ CreatureType Saproling))
  <|> try (ciString "Satyr" >> (return $ CreatureType Satyr))
  <|> try (ciString "Scarecrow" >> (return $ CreatureType Scarecrow))
  <|> try (ciString "Scorpion" >> (return $ CreatureType Scorpion))
  <|> try (ciString "Scout" >> (return $ CreatureType Scout))
  <|> try (ciString "Serf" >> (return $ CreatureType Serf))
  <|> try (ciString "Serpent" >> (return $ CreatureType Serpent))
  <|> try (ciString "Shade" >> (return $ CreatureType Shade))
  <|> try (ciString "Shaman" >> (return $ CreatureType Shaman))
  <|> try (ciString "Shapeshifter" >> (return $ CreatureType Shapeshifter))
  <|> try (ciString "Sheep" >> (return $ CreatureType Sheep))
  <|> try (ciString "Siren" >> (return $ CreatureType Siren))
  <|> try (ciString "Skeleton" >> (return $ CreatureType Skeleton))
  <|> try (ciString "Slith" >> (return $ CreatureType Slith))
  <|> try (ciString "Sliver" >> (return $ CreatureType Sliver))
  <|> try (ciString "Slug" >> (return $ CreatureType Slug))
  <|> try (ciString "Snake" >> (return $ CreatureType Snake))
  <|> try (ciString "Soldier" >> (return $ CreatureType Soldier))
  <|> try (ciString "Soltari" >> (return $ CreatureType Soltari))
  <|> try (ciString "Spawn" >> (return $ CreatureType Spawn))
  <|> try (ciString "Specter" >> (return $ CreatureType Specter))
  <|> try (ciString "Spellshaper" >> (return $ CreatureType Spellshaper))
  <|> try (ciString "Sphinx" >> (return $ CreatureType Sphinx))
  <|> try (ciString "Spider" >> (return $ CreatureType Spider))
  <|> try (ciString "Spike" >> (return $ CreatureType Spike))
  <|> try (ciString "Spirit" >> (return $ CreatureType Spirit))
  <|> try (ciString "Splinter" >> (return $ CreatureType Splinter))
  <|> try (ciString "Sponge" >> (return $ CreatureType Sponge))
  <|> try (ciString "Squid" >> (return $ CreatureType Squid))
  <|> try (ciString "Squirrel" >> (return $ CreatureType Squirrel))
  <|> try (ciString "Starfish" >> (return $ CreatureType Starfish))
  <|> try (ciString "Surrakar" >> (return $ CreatureType Surrakar))
  <|> try (ciString "Survivor" >> (return $ CreatureType Survivor))
  <|> try (ciString "Tetravite" >> (return $ CreatureType Tetravite))
  <|> try (ciString "Thalakos" >> (return $ CreatureType Thalakos))
  <|> try (ciString "Thopter" >> (return $ CreatureType Thopter))
  <|> try (ciString "Thrull" >> (return $ CreatureType Thrull))
  <|> try (ciString "Treefolk" >> (return $ CreatureType Treefolk))
  <|> try (ciString "Triskelavite" >> (return $ CreatureType Triskelavite))
  <|> try (ciString "Troll" >> (return $ CreatureType Troll))
  <|> try (ciString "Turtle" >> (return $ CreatureType Turtle))
  <|> try (ciString "Unicorn" >> (return $ CreatureType Unicorn))
  <|> try (ciString "Vampire" >> (return $ CreatureType Vampire))
  <|> try (ciString "Vedalken" >> (return $ CreatureType Vedalken))
  <|> try (ciString "Viashino" >> (return $ CreatureType Viashino))
  <|> try (ciString "Volver" >> (return $ CreatureType Volver))
  <|> try (ciString "Wall" >> (return $ CreatureType Wall))
  <|> try (ciString "Warrior" >> (return $ CreatureType Warrior))
  <|> try (ciString "Weird" >> (return $ CreatureType Weird))
  <|> try (ciString "Werewolf" >> (return $ CreatureType Werewolf))
  <|> try (ciString "Werewolves" >> (return $ CreatureType Werewolf))
  <|> try (ciString "Whale" >> (return $ CreatureType Whale))
  <|> try (ciString "Wizard" >> (return $ CreatureType Wizard))
  <|> try (ciString "Wolf" >> (return $ CreatureType Wolf))
  <|> try (ciString "Wolves" >> (return $ CreatureType Wolf))
  <|> try (ciString "Wolverine" >> (return $ CreatureType Wolverine))
  <|> try (ciString "Wombat" >> (return $ CreatureType Wombat))
  <|> try (ciString "Worm" >> (return $ CreatureType Worm))
  <|> try (ciString "Wraith" >> (return $ CreatureType Wraith))
  <|> try (ciString "Wurm" >> (return $ CreatureType Wurm))
  <|> try (ciString "Yeti" >> (return $ CreatureType Yeti))
  <|> try (ciString "Zombie" >> (return $ CreatureType Zombie))
  <|> try (ciString "Zubera" >> (return $ CreatureType Zubera))

-- TODO: Should this be a custom instance of Read instead?
-- TODO: Generalize to take the parser fn as argument: parseString
stringToSubtype :: String -> Subtype
stringToSubtype s = case (parse subtypeParser "" s) of
                      Left e -> error (show e)
                      Right xs -> xs

data ArtifactType = Contraption | Equipment | Fortification
                     deriving (Show, Eq)

data EnchantmentType = Aura | Curse | Shrine
                     deriving (Show, Eq)

data LandType = BasicLand BasicLandType
              | Desert | Gate | Lair | Locus | Mine | PowerPlant
              | Tower | Urzas
              deriving (Show, Eq)

data BasicLandType = Forest | Island | Mountain | Plains | Swamp
                   deriving (Show, Eq)

data PlaneswalkerType = Ajani | Ashiok | Bolas | Chandra | Domri | Elspeth
                      | Garruk | Gideon | Jace | Karn | Koth | Liliana
                      | Nissa | Ral | Sarkhan | Sorin | Tamiyo | Tezzeret
                      | Tibalt | Venser | Vraska | Xenagos
                      deriving (Show, Eq)
data SpellType = Arcane | Trap
               deriving (Show, Eq)

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
                  deriving (Show, Eq)
