module Debug where

import Control.Monad.State

import Game.MtG.GameEngine
import Game.MtG.Types

engine :: IO Game
engine = execStateT playGame testInitialGame

testInitialGame :: Game
testInitialGame = initialGame
  [ ("Player 1", replicate 9 plains ++ replicate 4 nykthos
              ++ replicate 4 elspeth
              ++ replicate 15 yokedOx ++ replicate 8 battlewiseValor)
  , ("Player 2", replicate 9 forest ++ replicate 8 island
              ++ replicate 15 leafcrownDryad ++ replicate 8 horizonChimera)
  ]

yokedOx :: Card
yokedOx = Card{_cardLayout = Normal, _cardTypeLine = "Creature \8212 Ox",
         _cardTypes = [Creature], _cardColors = [White],
         _cardMultiverseID = 373572, _cardName = "Yoked Ox", _cardNames = [],
         _cardSupertypes = [], _cardSubtypes = [CreatureType Ox],
         _cardCmc = Just 1, _cardRarity = Common, _cardArtist = "Ryan Yee",
         _cardPower = Just "0", _cardToughness = Just "4",
         _cardLoyalty = Nothing, _cardManaCost = Just [W],
         _cardRulesText = Nothing, _cardAbilities = [], _cardCardNumber = "37",
         _cardVariations = [], _cardImageName = "yoked ox",
         _cardWatermark = Nothing, _cardCardBorder = Nothing,
         _cardSetCode = "THS"}

plains :: Card
plains = Card{_cardLayout = Normal, _cardTypeLine = "Basic Land \8212 Plains",
         _cardTypes = [Land], _cardColors = [], _cardMultiverseID = 373654,
         _cardName = "Plains", _cardNames = [], _cardSupertypes = [Basic],
         _cardSubtypes = [LandType (BasicLand Plains)], _cardCmc = Nothing,
         _cardRarity = BasicLandRarity, _cardArtist = "Rob Alexander",
         _cardPower = Nothing, _cardToughness = Nothing, _cardLoyalty = Nothing,
         _cardManaCost = Nothing, _cardRulesText = Just "W",
         _cardAbilities =
           [ActivatedAbility [CTap] [AddMana Nothing (ManaSymbols [[W]])]
              Nothing],
         _cardCardNumber = "230", _cardVariations = [373533, 373582, 373700],
         _cardImageName = "plains1", _cardWatermark = Nothing,
         _cardCardBorder = Nothing, _cardSetCode = "THS"}

forest :: Card
forest = Card{_cardLayout = Normal, _cardTypeLine = "Basic Land \8212 Forest",
         _cardTypes = [Land], _cardColors = [], _cardMultiverseID = 373625,
         _cardName = "Forest", _cardNames = [], _cardSupertypes = [Basic],
         _cardSubtypes = [LandType (BasicLand Forest)], _cardCmc = Nothing,
         _cardRarity = BasicLandRarity, _cardArtist = "Steven Belledin",
         _cardPower = Nothing, _cardToughness = Nothing, _cardLoyalty = Nothing,
         _cardManaCost = Nothing, _cardRulesText = Just "G",
         _cardAbilities =
           [ActivatedAbility [CTap] [AddMana Nothing (ManaSymbols [[G]])]
              Nothing],
         _cardCardNumber = "247", _cardVariations = [373568, 373615, 373688],
         _cardImageName = "forest2", _cardWatermark = Nothing,
         _cardCardBorder = Nothing, _cardSetCode = "THS"}

island :: Card
island = Card{_cardLayout = Normal, _cardTypeLine = "Basic Land \8212 Island",
         _cardTypes = [Land], _cardColors = [], _cardMultiverseID = 373595,
         _cardName = "Island", _cardNames = [], _cardSupertypes = [Basic],
         _cardSubtypes = [LandType (BasicLand Island)], _cardCmc = Nothing,
         _cardRarity = BasicLandRarity, _cardArtist = "Steven Belledin",
         _cardPower = Nothing, _cardToughness = Nothing, _cardLoyalty = Nothing,
         _cardManaCost = Nothing, _cardRulesText = Just "U",
         _cardAbilities =
           [ActivatedAbility [CTap] [AddMana Nothing (ManaSymbols [[U]])]
              Nothing],
         _cardCardNumber = "235", _cardVariations = [373558, 373723, 373736],
         _cardImageName = "island2", _cardWatermark = Nothing,
         _cardCardBorder = Nothing, _cardSetCode = "THS"}

leafcrownDryad :: Card
leafcrownDryad = Card{_cardLayout = Normal,
         _cardTypeLine = "Enchantment Creature \8212 Nymph Dryad",
         _cardTypes = [Enchantment, Creature], _cardColors = [Green],
         _cardMultiverseID = 373523, _cardName = "Leafcrown Dryad",
         _cardNames = [], _cardSupertypes = [],
         _cardSubtypes = [CreatureType Nymph, CreatureType Dryad],
         _cardCmc = Just 2, _cardRarity = Common, _cardArtist = "Volkan Baga",
         _cardPower = Just "2", _cardToughness = Just "2",
         _cardLoyalty = Nothing, _cardManaCost = Just [CL 1, G],
         _cardRulesText =
           Just
             "Bestow {3}{G} (If you cast this card for its bestow cost, it's an Aura spell with enchant creature. It becomes a creature again if it's not attached to a creature.)\n\nReach\n\nEnchanted creature gets +2/+2 and has reach.",
         _cardAbilities =
           [KeywordAbility (Bestow [CMana [CL 3, G]]), KeywordAbility Reach,
            SpellAbility
              [ModifyPT (NoTarget Nothing [TMEnchantedPermanent])
                 (Plus (NumValue 2))
                 (Plus (NumValue 2))
                 Nothing,
               AddAbilities (NoTarget Nothing [TMIt]) [KeywordAbility Reach]
                 Nothing]],
         _cardCardNumber = "161", _cardVariations = [],
         _cardImageName = "leafcrown dryad", _cardWatermark = Nothing,
         _cardCardBorder = Nothing, _cardSetCode = "THS"}

horizonChimera :: Card
horizonChimera = Card{_cardLayout = Normal, _cardTypeLine = "Creature \8212 Chimera",
         _cardTypes = [Creature], _cardColors = [Blue, Green],
         _cardMultiverseID = 373738, _cardName = "Horizon Chimera",
         _cardNames = [], _cardSupertypes = [],
         _cardSubtypes = [CreatureType Chimera], _cardCmc = Just 4,
         _cardRarity = Uncommon, _cardArtist = "Sam Burley",
         _cardPower = Just "3", _cardToughness = Just "2",
         _cardLoyalty = Nothing, _cardManaCost = Just [CL 2, G, U],
         _cardRulesText =
           Just
             "Flash (You may cast this spell any time you could cast an instant.)\n\nFlying, trample\n\nWhenever you draw a card, you gain 1 life.",
         _cardAbilities =
           [KeywordAbility Flash, KeywordAbility Flying, KeywordAbility Trample,
            TriggeredAbility (TEOther "you draw a card")
              [GainLife (NoTarget Nothing [TMPlayer You]) (NumValue 1)]
              Nothing],
         _cardCardNumber = "194", _cardVariations = [],
         _cardImageName = "horizon chimera", _cardWatermark = Nothing,
         _cardCardBorder = Nothing, _cardSetCode = "THS"}

battlewiseValor :: Card
battlewiseValor = Card{_cardLayout = Normal, _cardTypeLine = "Instant",
         _cardTypes = [Instant], _cardColors = [White],
         _cardMultiverseID = 373627, _cardName = "Battlewise Valor",
         _cardNames = [], _cardSupertypes = [], _cardSubtypes = [],
         _cardCmc = Just 2, _cardRarity = Common, _cardArtist = "Zack Stella",
         _cardPower = Nothing, _cardToughness = Nothing, _cardLoyalty = Nothing,
         _cardManaCost = Just [CL 1, W],
         _cardRulesText =
           Just
             "Target creature gets +2/+2 until end of turn. Scry 1. (Look at the top card of your library. You may put that card on the bottom of your library.)",
         _cardAbilities =
           [SpellAbility
              [ModifyPT
                 (Target (Exactly (AnyCount (NumValue 1)))
                    [TMPermanent
                       (PermanentMatch Nothing [] (CMColors []) CardOrToken
                          (PermanentTypeMatch [] [Non True Creature] [])
                          []
                          Nothing
                          Nothing
                          Nothing)])
                 (Plus (NumValue 2))
                 (Plus (NumValue 2))
                 (Just
                    (DurationUntil (TEAt (Just EachPlayer) Nothing Cleanup))),
               Scry (NumValue 1)]],
         _cardCardNumber = "1", _cardVariations = [],
         _cardImageName = "battlewise valor", _cardWatermark = Nothing,
         _cardCardBorder = Nothing, _cardSetCode = "THS"}

nykthos :: Card
nykthos = Card{_cardLayout = Normal, _cardTypeLine = "Legendary Land",
         _cardTypes = [Land], _cardColors = [], _cardMultiverseID = 373713,
         _cardName = "Nykthos, Shrine to Nyx", _cardNames = [],
         _cardSupertypes = [Legendary], _cardSubtypes = [], _cardCmc = Nothing,
         _cardRarity = Rare, _cardArtist = "Jung Park", _cardPower = Nothing,
         _cardToughness = Nothing, _cardLoyalty = Nothing,
         _cardManaCost = Nothing,
         _cardRulesText =
           Just
             "{T}: Add {1} to your mana pool.\n\n{2}, {T}: Choose a color. Add to your mana pool an amount of mana of that color equal to your devotion to that color. (Your devotion to a color is the number of mana symbols of that color in the mana costs of permanents you control.)",
         _cardAbilities =
           [ActivatedAbility [CTap] [AddMana Nothing (ManaSymbols [[CL 1]])]
              Nothing,
            ActivatedAbility [CMana [CL 2], CTap]
              [OtherEffect "Choose a color",
               AddMana
                 (Just
                    (Exactly
                       (AnyCount (NumVariable "your devotion to that color"))))
                 ManaThatColor]
              Nothing],
         _cardCardNumber = "223", _cardVariations = [],
         _cardImageName = "nykthos, shrine to nyx", _cardWatermark = Nothing,
         _cardCardBorder = Nothing, _cardSetCode = "THS"}

elspeth :: Card
elspeth = Card{_cardLayout = Normal,
         _cardTypeLine = "Planeswalker \8212 Elspeth",
         _cardTypes = [Planeswalker], _cardColors = [White],
         _cardMultiverseID = 373649, _cardName = "Elspeth, Sun's Champion",
         _cardNames = [], _cardSupertypes = [],
         _cardSubtypes = [PlaneswalkerType Elspeth], _cardCmc = Just 6,
         _cardRarity = MythicRare, _cardArtist = "Eric Deschamps",
         _cardPower = Nothing, _cardToughness = Nothing, _cardLoyalty = Just 4,
         _cardManaCost = Just [CL 4, W, W],
         _cardRulesText =
           Just
             "+1: Put three 1/1 white Soldier creature tokens onto the battlefield.\n\n-3: Destroy all creatures with power 4 or greater.\n\n-7: You get an emblem with \"Creatures you control get +2/+2 and have flying.\"",
         _cardAbilities =
           [ActivatedAbility [CLoyalty (Plus (NumValue 1))]
              [PutTokens (NoTarget Nothing [TMPlayer You]) (NumValue 3)
                 (NumValue 1)
                 (NumValue 1)
                 (PermanentMatch Nothing [] (CMColors [Non True White])
                    CardOrToken
                    (PermanentTypeMatch [] [Non True Creature]
                       [Non True (CreatureType Soldier)])
                    []
                    Nothing
                    Nothing
                    Nothing)
                 Nothing]
              Nothing,
            ActivatedAbility [CLoyalty (Minus (NumValue 3))]
              [Destroy
                 (NoTarget (Just (Exactly (AnyCount All)))
                    [TMPermanent
                       (PermanentMatch Nothing [] (CMColors []) CardOrToken
                          (PermanentTypeMatch [] [Non True Creature] [])
                          []
                          (Just (QPower (AtLeast (AnyCount (NumValue 4)))))
                          Nothing
                          Nothing)])]
              Nothing,
            ActivatedAbility [CLoyalty (Minus (NumValue 7))]
              [GetEmblem
                 [SpellAbility
                    [ModifyPT
                       (NoTarget Nothing
                          [TMPermanent
                             (PermanentMatch Nothing [] (CMColors [])
                                CardOrToken
                                (PermanentTypeMatch [] [Non True Creature] [])
                                []
                                Nothing
                                Nothing
                                (Just (Control You)))])
                       (Plus (NumValue 2))
                       (Plus (NumValue 2))
                       Nothing,
                     AddAbilities (NoTarget Nothing [TMIt])
                       [KeywordAbility Flying]
                       Nothing]]]
              Nothing],
         _cardCardNumber = "9", _cardVariations = [],
         _cardImageName = "elspeth, sun's champion", _cardWatermark = Nothing,
         _cardCardBorder = Nothing, _cardSetCode = "THS"}

