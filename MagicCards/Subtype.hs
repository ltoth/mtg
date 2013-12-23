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
) where

import Control.Applicative
import Control.Monad
import Data.Aeson (FromJSON, parseJSON, Value(..), (.:), (.:?))

data Subtype = ArtifactType ArtifactType
             | EnchantmentType EnchantmentType
             | LandType LandType
             | PlaneswalkerType PlaneswalkerType
             | SpellType SpellType
             | CreatureType CreatureType
            deriving (Show, Eq)
instance FromJSON Subtype where
    parseJSON (String s)
      -- Artifact types
      | s == "Contraption" = return $ ArtifactType Contraption
      | s == "Equipment" = return $ ArtifactType Equipment
      | s == "Fortification" = return $ ArtifactType Fortification

      -- Enchantment types
      | s == "Aura" = return $ EnchantmentType Aura
      | s == "Curse" = return $ EnchantmentType Curse
      | s == "Shrine" = return $ EnchantmentType Shrine

      -- Basic land types
      | s == "Forest" = return $ LandType $ BasicLand Forest
      | s == "Island" = return $ LandType $ BasicLand Island
      | s == "Mountain" = return $ LandType $ BasicLand Mountain
      | s == "Plains" = return $ LandType $ BasicLand Plains
      | s == "Swamp" = return $ LandType $ BasicLand Swamp

      -- Other land types
      | s == "Desert" = return $ LandType Desert
      | s == "Gate" = return $ LandType Gate
      | s == "Lair" = return $ LandType Lair
      | s == "Locus" = return $ LandType Locus
      | s == "Mine" = return $ LandType Mine
      | s == "Power-Plant" = return $ LandType PowerPlant
      | s == "Tower" = return $ LandType Tower
      | s == "Urza's" = return $ LandType Urzas

      -- Planeswalker types
      | s == "Ajani" = return $ PlaneswalkerType Ajani
      | s == "Ashiok" = return $ PlaneswalkerType Ashiok
      | s == "Bolas" = return $ PlaneswalkerType Bolas
      | s == "Chandra" = return $ PlaneswalkerType Chandra
      | s == "Domri" = return $ PlaneswalkerType Domri
      | s == "Elspeth" = return $ PlaneswalkerType Elspeth
      | s == "Garruk" = return $ PlaneswalkerType Garruk
      | s == "Gideon" = return $ PlaneswalkerType Gideon
      | s == "Jace" = return $ PlaneswalkerType Jace
      | s == "Karn" = return $ PlaneswalkerType Karn
      | s == "Koth" = return $ PlaneswalkerType Koth
      | s == "Liliana" = return $ PlaneswalkerType Liliana
      | s == "Nissa" = return $ PlaneswalkerType Nissa
      | s == "Ral" = return $ PlaneswalkerType Ral
      | s == "Sarkhan" = return $ PlaneswalkerType Sarkhan
      | s == "Sorin" = return $ PlaneswalkerType Sorin
      | s == "Tamiyo" = return $ PlaneswalkerType Tamiyo
      | s == "Tezzeret" = return $ PlaneswalkerType Tezzeret
      | s == "Tibalt" = return $ PlaneswalkerType Tibalt
      | s == "Venser" = return $ PlaneswalkerType Venser
      | s == "Vraska" = return $ PlaneswalkerType Vraska
      | s == "Xenagos" = return $ PlaneswalkerType Xenagos

      -- Spell types
      | s == "Arcane" = return $ SpellType Arcane
      | s == "Trap" = return $ SpellType Trap

      -- Creature types
      | s == "Advisor" = return $ CreatureType Advisor
      | s == "Ally" = return $ CreatureType Ally
      | s == "Angel" = return $ CreatureType Angel
      | s == "Anteater" = return $ CreatureType Anteater
      | s == "Antelope" = return $ CreatureType Antelope
      | s == "Ape" = return $ CreatureType Ape
      | s == "Archer" = return $ CreatureType Archer
      | s == "Archon" = return $ CreatureType Archon
      | s == "Artificer" = return $ CreatureType Artificer
      | s == "Assassin" = return $ CreatureType Assassin
      | s == "Assembly-Worker" = return $ CreatureType AssemblyWorker
      | s == "Atog" = return $ CreatureType Atog
      | s == "Aurochs" = return $ CreatureType Aurochs
      | s == "Avatar" = return $ CreatureType Avatar
      | s == "Badger" = return $ CreatureType Badger
      | s == "Barbarian" = return $ CreatureType Barbarian
      | s == "Basilisk" = return $ CreatureType Basilisk
      | s == "Bat" = return $ CreatureType Bat
      | s == "Bear" = return $ CreatureType Bear
      | s == "Beast" = return $ CreatureType Beast
      | s == "Beeble" = return $ CreatureType Beeble
      | s == "Berserker" = return $ CreatureType Berserker
      | s == "Bird" = return $ CreatureType Bird
      | s == "Blinkmoth" = return $ CreatureType Blinkmoth
      | s == "Boar" = return $ CreatureType Boar
      | s == "Bringer" = return $ CreatureType Bringer
      | s == "Brushwagg" = return $ CreatureType Brushwagg
      | s == "Camarid" = return $ CreatureType Camarid
      | s == "Camel" = return $ CreatureType Camel
      | s == "Caribou" = return $ CreatureType Caribou
      | s == "Carrier" = return $ CreatureType Carrier
      | s == "Cat" = return $ CreatureType Cat
      | s == "Centaur" = return $ CreatureType Centaur
      | s == "Cephalid" = return $ CreatureType Cephalid
      | s == "Chimera" = return $ CreatureType Chimera
      | s == "Citizen" = return $ CreatureType Citizen
      | s == "Cleric" = return $ CreatureType Cleric
      | s == "Cockatrice" = return $ CreatureType Cockatrice
      | s == "Construct" = return $ CreatureType Construct
      | s == "Coward" = return $ CreatureType Coward
      | s == "Crab" = return $ CreatureType Crab
      | s == "Crocodile" = return $ CreatureType Crocodile
      | s == "Cyclops" = return $ CreatureType Cyclops
      | s == "Dauthi" = return $ CreatureType Dauthi
      | s == "Demon" = return $ CreatureType Demon
      | s == "Deserter" = return $ CreatureType Deserter
      | s == "Devil" = return $ CreatureType Devil
      | s == "Djinn" = return $ CreatureType Djinn
      | s == "Dragon" = return $ CreatureType Dragon
      | s == "Drake" = return $ CreatureType Drake
      | s == "Dreadnought" = return $ CreatureType Dreadnought
      | s == "Drone" = return $ CreatureType Drone
      | s == "Druid" = return $ CreatureType Druid
      | s == "Dryad" = return $ CreatureType Dryad
      | s == "Dwarf" = return $ CreatureType Dwarf
      | s == "Efreet" = return $ CreatureType Efreet
      | s == "Elder" = return $ CreatureType Elder
      | s == "Eldrazi" = return $ CreatureType Eldrazi
      | s == "Elemental" = return $ CreatureType Elemental
      | s == "Elephant" = return $ CreatureType Elephant
      | s == "Elf" = return $ CreatureType Elf
      | s == "Elk" = return $ CreatureType Elk
      | s == "Eye" = return $ CreatureType Eye
      | s == "Faerie" = return $ CreatureType Faerie
      | s == "Ferret" = return $ CreatureType Ferret
      | s == "Fish" = return $ CreatureType Fish
      | s == "Flagbearer" = return $ CreatureType Flagbearer
      | s == "Fox" = return $ CreatureType Fox
      | s == "Frog" = return $ CreatureType Frog
      | s == "Fungus" = return $ CreatureType Fungus
      | s == "Gargoyle" = return $ CreatureType Gargoyle
      | s == "Germ" = return $ CreatureType Germ
      | s == "Giant" = return $ CreatureType Giant
      | s == "Gnome" = return $ CreatureType Gnome
      | s == "Goat" = return $ CreatureType Goat
      | s == "Goblin" = return $ CreatureType Goblin
      | s == "God" = return $ CreatureType God
      | s == "Golem" = return $ CreatureType Golem
      | s == "Gorgon" = return $ CreatureType Gorgon
      | s == "Graveborn" = return $ CreatureType Graveborn
      | s == "Gremlin" = return $ CreatureType Gremlin
      | s == "Griffin" = return $ CreatureType Griffin
      | s == "Hag" = return $ CreatureType Hag
      | s == "Harpy" = return $ CreatureType Harpy
      | s == "Hellion" = return $ CreatureType Hellion
      | s == "Hippo" = return $ CreatureType Hippo
      | s == "Hippogriff" = return $ CreatureType Hippogriff
      | s == "Homarid" = return $ CreatureType Homarid
      | s == "Homunculus" = return $ CreatureType Homunculus
      | s == "Horror" = return $ CreatureType Horror
      | s == "Horse" = return $ CreatureType Horse
      | s == "Hound" = return $ CreatureType Hound
      | s == "Human" = return $ CreatureType Human
      | s == "Hydra" = return $ CreatureType Hydra
      | s == "Hyena" = return $ CreatureType Hyena
      | s == "Illusion" = return $ CreatureType Illusion
      | s == "Imp" = return $ CreatureType Imp
      | s == "Incarnation" = return $ CreatureType Incarnation
      | s == "Insect" = return $ CreatureType Insect
      | s == "Jellyfish" = return $ CreatureType Jellyfish
      | s == "Juggernaut" = return $ CreatureType Juggernaut
      | s == "Kavu" = return $ CreatureType Kavu
      | s == "Kirin" = return $ CreatureType Kirin
      | s == "Kithkin" = return $ CreatureType Kithkin
      | s == "Knight" = return $ CreatureType Knight
      | s == "Kobold" = return $ CreatureType Kobold
      | s == "Kor" = return $ CreatureType Kor
      | s == "Kraken" = return $ CreatureType Kraken
      | s == "Lammasu" = return $ CreatureType Lammasu
      | s == "Leech" = return $ CreatureType Leech
      | s == "Leviathan" = return $ CreatureType Leviathan
      | s == "Lhurgoyf" = return $ CreatureType Lhurgoyf
      | s == "Licid" = return $ CreatureType Licid
      | s == "Lizard" = return $ CreatureType Lizard
      | s == "Manticore" = return $ CreatureType Manticore
      | s == "Masticore" = return $ CreatureType Masticore
      | s == "Mercenary" = return $ CreatureType Mercenary
      | s == "Merfolk" = return $ CreatureType Merfolk
      | s == "Metathran" = return $ CreatureType Metathran
      | s == "Minion" = return $ CreatureType Minion
      | s == "Minotaur" = return $ CreatureType Minotaur
      | s == "Monger" = return $ CreatureType Monger
      | s == "Mongoose" = return $ CreatureType Mongoose
      | s == "Monk" = return $ CreatureType Monk
      | s == "Moonfolk" = return $ CreatureType Moonfolk
      | s == "Mutant" = return $ CreatureType Mutant
      | s == "Myr" = return $ CreatureType Myr
      | s == "Mystic" = return $ CreatureType Mystic
      | s == "Nautilus" = return $ CreatureType Nautilus
      | s == "Nephilim" = return $ CreatureType Nephilim
      | s == "Nightmare" = return $ CreatureType Nightmare
      | s == "Nightstalker" = return $ CreatureType Nightstalker
      | s == "Ninja" = return $ CreatureType Ninja
      | s == "Noggle" = return $ CreatureType Noggle
      | s == "Nomad" = return $ CreatureType Nomad
      | s == "Nymph" = return $ CreatureType Nymph
      | s == "Octopus" = return $ CreatureType Octopus
      | s == "Ogre" = return $ CreatureType Ogre
      | s == "Ooze" = return $ CreatureType Ooze
      | s == "Orb" = return $ CreatureType Orb
      | s == "Orc" = return $ CreatureType Orc
      | s == "Orgg" = return $ CreatureType Orgg
      | s == "Ouphe" = return $ CreatureType Ouphe
      | s == "Ox" = return $ CreatureType Ox
      | s == "Oyster" = return $ CreatureType Oyster
      | s == "Pegasus" = return $ CreatureType Pegasus
      | s == "Pentavite" = return $ CreatureType Pentavite
      | s == "Pest" = return $ CreatureType Pest
      | s == "Phelddagrif" = return $ CreatureType Phelddagrif
      | s == "Phoenix" = return $ CreatureType Phoenix
      | s == "Pincher" = return $ CreatureType Pincher
      | s == "Pirate" = return $ CreatureType Pirate
      | s == "Plant" = return $ CreatureType Plant
      | s == "Praetor" = return $ CreatureType Praetor
      | s == "Prism" = return $ CreatureType Prism
      | s == "Rabbit" = return $ CreatureType Rabbit
      | s == "Rat" = return $ CreatureType Rat
      | s == "Rebel" = return $ CreatureType Rebel
      | s == "Reflection" = return $ CreatureType Reflection
      | s == "Rhino" = return $ CreatureType Rhino
      | s == "Rigger" = return $ CreatureType Rigger
      | s == "Rogue" = return $ CreatureType Rogue
      | s == "Sable" = return $ CreatureType Sable
      | s == "Salamander" = return $ CreatureType Salamander
      | s == "Samurai" = return $ CreatureType Samurai
      | s == "Sand" = return $ CreatureType Sand
      | s == "Saproling" = return $ CreatureType Saproling
      | s == "Satyr" = return $ CreatureType Satyr
      | s == "Scarecrow" = return $ CreatureType Scarecrow
      | s == "Scorpion" = return $ CreatureType Scorpion
      | s == "Scout" = return $ CreatureType Scout
      | s == "Serf" = return $ CreatureType Serf
      | s == "Serpent" = return $ CreatureType Serpent
      | s == "Shade" = return $ CreatureType Shade
      | s == "Shaman" = return $ CreatureType Shaman
      | s == "Shapeshifter" = return $ CreatureType Shapeshifter
      | s == "Sheep" = return $ CreatureType Sheep
      | s == "Siren" = return $ CreatureType Siren
      | s == "Skeleton" = return $ CreatureType Skeleton
      | s == "Slith" = return $ CreatureType Slith
      | s == "Sliver" = return $ CreatureType Sliver
      | s == "Slug" = return $ CreatureType Slug
      | s == "Snake" = return $ CreatureType Snake
      | s == "Soldier" = return $ CreatureType Soldier
      | s == "Soltari" = return $ CreatureType Soltari
      | s == "Spawn" = return $ CreatureType Spawn
      | s == "Specter" = return $ CreatureType Specter
      | s == "Spellshaper" = return $ CreatureType Spellshaper
      | s == "Sphinx" = return $ CreatureType Sphinx
      | s == "Spider" = return $ CreatureType Spider
      | s == "Spike" = return $ CreatureType Spike
      | s == "Spirit" = return $ CreatureType Spirit
      | s == "Splinter" = return $ CreatureType Splinter
      | s == "Sponge" = return $ CreatureType Sponge
      | s == "Squid" = return $ CreatureType Squid
      | s == "Squirrel" = return $ CreatureType Squirrel
      | s == "Starfish" = return $ CreatureType Starfish
      | s == "Surrakar" = return $ CreatureType Surrakar
      | s == "Survivor" = return $ CreatureType Survivor
      | s == "Tetravite" = return $ CreatureType Tetravite
      | s == "Thalakos" = return $ CreatureType Thalakos
      | s == "Thopter" = return $ CreatureType Thopter
      | s == "Thrull" = return $ CreatureType Thrull
      | s == "Treefolk" = return $ CreatureType Treefolk
      | s == "Triskelavite" = return $ CreatureType Triskelavite
      | s == "Troll" = return $ CreatureType Troll
      | s == "Turtle" = return $ CreatureType Turtle
      | s == "Unicorn" = return $ CreatureType Unicorn
      | s == "Vampire" = return $ CreatureType Vampire
      | s == "Vedalken" = return $ CreatureType Vedalken
      | s == "Viashino" = return $ CreatureType Viashino
      | s == "Volver" = return $ CreatureType Volver
      | s == "Wall" = return $ CreatureType Wall
      | s == "Warrior" = return $ CreatureType Warrior
      | s == "Weird" = return $ CreatureType Weird
      | s == "Werewolf" = return $ CreatureType Werewolf
      | s == "Whale" = return $ CreatureType Whale
      | s == "Wizard" = return $ CreatureType Wizard
      | s == "Wolf" = return $ CreatureType Wolf
      | s == "Wolverine" = return $ CreatureType Wolverine
      | s == "Wombat" = return $ CreatureType Wombat
      | s == "Worm" = return $ CreatureType Worm
      | s == "Wraith" = return $ CreatureType Wraith
      | s == "Wurm" = return $ CreatureType Wurm
      | s == "Yeti" = return $ CreatureType Yeti
      | s == "Zombie" = return $ CreatureType Zombie
      | s == "Zubera" = return $ CreatureType Zubera

      | otherwise = fail "Invalid subtype string specified"
    parseJSON _ = fail "Could not parse subtype"

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
