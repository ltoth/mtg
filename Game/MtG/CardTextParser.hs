module Game.MtG.CardTextParser
( parseAndSetAbilities
) where

import Control.Applicative
import Control.Lens hiding (noneOf)
import Control.Monad
import qualified Data.Text as T
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Text.Regex

import Text.Parsec.Char.Extra (ciString)
import Text.ParserCombinators.Parsec.Extra (sepBy2)

import Game.MtG.Types
import Game.MtG.JSONParser ( manaCostParser
                           , manaSymbolParser
                           , colorParser
                           , supertypeParser
                           , typeParser
                           , subtypeParser )

parseAndSetAbilities :: Card -> Either String Card
parseAndSetAbilities c = case parsedAbilities of
                           Just (Right as) -> Right $ c & abilities .~ as
                           Just (Left  e)  -> Left e
                           Nothing         -> Right c
  where parsedAbilities = textToAbilities <$>
                          removeReminder <$>
                          replaceThis c

removeReminder :: RulesText -> RulesText
-- FIXME: Should not be greedy
removeReminder t = T.pack $ subRegex (mkRegex " *\\([^)]+\\) *")
                   (subRegex (mkRegex "^\\([^)]+\\)\n\n") (T.unpack t) "") ""

replaceThis :: Card -> Maybe RulesText
replaceThis c =
    T.replace shortName "{This}"
    . T.replace (c ^. name) "{This}"
    <$> c ^. rulesText
    where shortName = T.takeWhile (/= ',')  (c ^. name)
          -- This handles the THS Gods, Tymaret, Jarad, etc.
          -- since only their first names are used in ability text

textToAbilities :: RulesText -> Either String [Ability]
textToAbilities ct = parse paras s s & _Left %~ show
  where s = T.unpack ct
                -- FIXME: Perhaps we shouldn't flatten the list, so
                -- that when Artisan's Sorrow has an illegal target,
                -- we know not to resolve Scry 2. Those effects are
                -- one ability.
        paras = concat <$> abilityPara `sepBy` string "\n\n" <* eof
        abilityPara = try (keyword `sepBy1` commas)
                  <|> (optional abilityWord >>
                       many1 (try additional
                             <|> try alternative
                             <|> try alternativeFree
                             <|> try activated
                             <|> try triggered
                             <|> try basicLand
                             <|> spell))
        commas = try (string ", ") <|> string ","
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
                            <* optional (many (oneOf ". "))

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
        condition = T.pack <$> many (noneOf ",\n")

        triggered =
          do
            event <- trigEvent
            string ", "
            cond <- optionMaybe $ try (string "if " *> conditions <* string ", ")
            es <- fxs
            return $ TriggeredAbility event es cond

        trigEvent =
              try (ciString "At " >> turnEvent)
          <|> try (ciString "Whenever " >> TEOther . T.pack <$> many (noneOf ",\n"))
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
                     <*> stepParser)
               <|> try (DurationEachTurn <$ ciString "each turn")
               <|> try (DurationEachCombat <$ ciString "each combat"))

        -- TODO: first check "at the beginning of combat on your turn"
        -- (Battle-Rattle Shaman)
        -- TODO: first check "at the beginning of each of your main phases"
        -- (Carpet of Flowers)
        turnEvent = try (TEAt <$ ciString "the beginning of "
                      <*> optionMaybe playerMatch
                      <*> optionMaybe next
                      <*> stepParser)
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
                    <|> TEOther . T.pack <$> many (noneOf ",\n")

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
            >> return PMPlayer)
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

        stepParser =
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

        fxs =
          -- Syntax that combines PT modifying and ability gaining
          try (do
                  optional (ciString "each ")
                  ts <- targets
                  pt <- modifyPTPartial
                  ciString " and "
                  ks <- addAbilitiesPartial
                  d <- optionMaybe duration
                  return [uncurry (ModifyPT ts) pt d,
                          AddAbilities (NoTarget Nothing [TMIt]) ks d])
                  <* optional numVariableConsume
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
                  <|> try (ciString "have") <|> ciString "has"
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
          <|> try (ZoneIt <$ ciString "it")

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
              optional (try $ ciString "Then " <|> ciString "and ") *>
              (try (OptionalEffect <$> playerMatch
                         <*> try (ciString "may " *> effect))
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
                         <*> (try ((ciString "return" <|> ciString "put")
                             >> optional (string "s") >> string " ") *> targets)
                         <*> optionMaybe (try $ string "from " >> zone)
                         <*> optionMaybe (try fromAmong)
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
                         <*> try ((ciString "lose" >> optional (string "s")
                             >> string " ") *> numberParser
                             <* (optional (string " ") >> string "life")))
              <|> try (GainLife <$> targets
                         <*> try ((ciString "gain" >> optional (string "s")
                             >> string " ") *> numberParser
                             <* (optional (string " ") >> string "life")))
              <|> try (PayLife <$> (ciString "Pay " *> numberParser
                             <* (optional (string " ") >> string "life")))
              <|> try (AddAbilities <$> targets
                        <*> addAbilitiesPartial
                        <*> optionMaybe duration)
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
                       <*> optionMaybe duration)
              <|> try (DealDamage <$> targets
                         <*> try ((ciString "deal" >> optional (string "s")
                             >> string " ") *> numberParser
                             <* (optional (string " ") >> string "damage "))
                         <*> (optionMaybe divided <* optional (string "to "))
                         <*> targets)
              <|> try (DrawCard <$> optionPlayerYou
                         <*> try ((ciString "draw" >> optional (string "s")
                             >> string " ") *> numberParser
                             <* (optional (string " ") >> string "card"
                             >> optional (string "s"))))
              <|> try (Sacrifice <$> optionPlayerYou
                         <*> try ((ciString "sacrifice" >> optional (string "s")
                             >> string " ") *> targets))
              <|> try (Discard <$> optionPlayerYou
                         <*> try ((ciString "discard" >> optional (string "s")
                             >> string " ") *> targets))
              <|> try (Regenerate <$ ciString "regenerate " <*> targets)
              <|> try (GainControl <$> optionPlayerYou
                         <*> try ((ciString "gain" >> optional (string "s")
                             >> string " control of ") *> targets)
                         <*> optionMaybe duration)
              <|> try (ciString "Remove " >> RemoveCounters <$> countRange
                         <*> try ((optional (string " ") *>
                         optionMaybe (T.pack <$> many1 (noneOf " \n")) <* optional (string " "))
                         <* ciString "counter" <* optional (string "s")
                         <* ciString " from ") <*> targets)
              <|> try (ciString "Put " >> PutCounters <$> countRange
                         <*> try ((optional (string " ") *>
                         optionMaybe (T.pack <$> many1 (noneOf " \n")) <* optional (string " "))
                         <* ciString "counter" <* optional (string "s")
                         <* ciString " on ") <*> targets)
              <|> try (PutTokens <$> optionPlayerYou
                         <*> ((ciString "put" >> optional (string "s")
                             >> string " ") *> numberParser)
                         <*> (string " " *> explicitNumber)
                         <*> (string "/" *> explicitNumber)
                         <*> (string " " *> permanentMatch)
                         <* optional (string " ") <* string "onto the battlefield"
                         <*> optionMaybe (try $ string " with " *> quotedAbilities))
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
                         <*> try ((optional (string " ") *>
                         optionMaybe (T.pack <$> many1 (noneOf " \n")) <* optional (string " "))
                         <* ciString "counter" <* optional (string "s")
                         <* ciString " on it"))
              <|> try (GetEmblem <$ ciString "You get an emblem with "
                         <*> quotedAbilities)
              <|> try (Monstrosity <$ ciString "Monstrosity "
                         <*> explicitNumber)
              <|> try (Scry <$ ciString "Scry " <*> explicitNumber)
              <|> (OtherEffect . T.pack <$> many1 (noneOf ".\n\""))
              ) <* optional numVariableConsume
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
          <*> (string ": " *> fxs)
          <*> optionMaybe activationInst

        -- FIXME: This will only work once all effects are accounted for
        -- and we take out OtherEffect
        activationInst = try $ string "Activate this ability only "
                               *> (T.pack <$> many1 (noneOf "\n"))

        totalCost = abilityCost `sepBy1` andSep
        andSep = try (string ", and ")
                     <|> try (string ", ")
                     <|> try (string " and ")
                     <|> string ","  -- FIXME: Is this necessary?
        abilityCost = try (string "{T}" >> return CTap)
                  <|> try (string "{Q}" >> return CUntap)
                  <|> try (optional (ciString "Pay ") >>
                        CMana <$> manaCostParser)
                  <|> try (CLoyalty <$> numChange)
                  <|> try (string "0" >> return (CLoyalty (Plus (NumValue 0))))
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
                          return ((OtherCount . NumValue) 1))
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
                            noneOf ",.\n" `manyTill` try (string "equal to ")
                            NumVariable . T.pack <$> many1 (noneOf ",.\n")))
                  <|> try explicitNumber

        explicitNumber = try (do
                           try (string "X")
                           lookAhead $ try (do
                             noneOf ".\n" `manyTill` try (string ", where X is ")
                             NumVariable . T.pack <$> many1 (noneOf ".\n")))
                  <|> try (NumValueX <$ string "X")
                  <|> try (NumValue 1 <$ string "an")
                  <|> try (NumValue 1 <$ string "a")
                  <|> try (NumValue 2 <$ string "both")
                  <|> try (NumValue 11 <$ string "eleven")
                  <|> try (NumValue 12 <$ string "twelve")
                  <|> try (NumValue 13 <$ string "thirteen")
                  <|> try (NumValue 14 <$ string "fourteen")
                  <|> try (NumValue 15 <$ string "fifteen")
                  <|> try (NumValue 16 <$ string "sixteen")
                  <|> try (NumValue 17 <$ string "seventeen")
                  <|> try (NumValue 18 <$ string "eighteen")
                  <|> try (NumValue 19 <$ string "nineteen")
                  <|> try (NumValue 20 <$ string "twenty")
                  <|> try (NumValue 1 <$ string "one")
                  <|> try (NumValue 2 <$ string "two")
                  <|> try (NumValue 3 <$ string "three")
                  <|> try (NumValue 4 <$ string "four")
                  <|> try (NumValue 5 <$ string "five")
                  <|> try (NumValue 6 <$ string "six")
                  <|> try (NumValue 7 <$ string "seven")
                  <|> try (NumValue 8 <$ string "eight")
                  <|> try (NumValue 9 <$ string "nine")
                  <|> try (NumValue 10 <$ string "ten")
                  <|> try ((NumValue . read) <$> many1 digit)

        -- used to consume this input, normally seen using lookAhead
        numVariableConsume =
              try (optional (string " ") *> string "equal to " >> many1 (noneOf ".\n"))
          <|> try (string ", where X is " >> many1 (noneOf ".\n"))

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
              n <- optionMaybe (try $ countRange <* string " "
                                  <* optional (string "of "))
              tms <- targetMatch `sepBy1` andOrSep'
              return $ NoTarget n tms)

        -- FIXME: Remove this and deal with consuming trailing spaces
        -- in permanentTypeMatch better
        andOrSep' = try (string ", and/or ")
               <|> try (string ", and " <* notFollowedBy effect)
               <|> try (string ", or ")
               <|> try (string ", " <* notFollowedBy effect)
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

        theRest = try (TMTheRest <$ ciString "the rest")
          <* optional (string " ")  -- to match permanentType's behavior

        cardMatch =
          (try (TopCardsOfLibrary <$ ciString "the top "
                 <*> option (NumValue 1) explicitNumber
                 <* optional (string " ") <* string "card"
                 <* optional (string "s") <* string " of " <*> zone)
          <|> try (CardMatch <$> (permanentTypeMatch `sepBy` orSep')
                 <* string "card" <* optional (string "s")
                 <*> optionMaybe withQuality
                 <*> optionMaybe (try (string " in " *> zone))))
          <* optional (string " ")  -- to match permanentType's behavior

        spellMatch =
          try (SpellMatch <$> colorMatch
                  <*> (permanentTypeMatch `sepBy` orSep')
                  <* string "spell" <* optional (string "s"))
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
                         return $ Own p)
                 <|> try (do
                         optional (string " ")
                         p <- playerMatch
                         string "control"
                         optional (string "s")
                         return $ Control p)
                 ) <* optional (string " ")

        nonParser = option True (try (do
                                string "non"
                                optional (string "-")
                                return False))

        cardNamed = try (do
          optional (string " ")
          string "named "
          T.pack <$> many1 (noneOf ",:;.\n")) -- FIXME: Actually match against
          -- possible card names, not just as strings, since
          -- this doesn't know when to stop properly, i.e. Kher Keep

        permanentTypeMatch =
              try (string "permanent" >> optional (string "s") >>
                return PTMPermanent)
          <|> try (string "token" >> optional (string "s") >>
                return PTMToken)
          <|> try (do
                super <- try (Non <$> nonParser <*> supertypeParser <* optional (string "s"))
                        `sepEndBy` string " "
                sub <- try (Non <$> nonParser <*> subtypeParser <* optional (string "s"))
                      `sepEndBy` string " "
                t <- try (Non <$> nonParser <*> typeParser <* optional (string "s"))
                    `sepEndBy` string " "
                optional (try (optional (string " ") *> string "permanent" <* optional (string "s")))
                optional (try (optional (string " ") *> string "token" <* optional (string "s")))
                optional (string " ")
                when (null super && null sub && null t)
                  (fail "Did not match any permanent type")
                return $ PermanentTypeMatch super t sub)

        spell = SpellAbility <$> fxs

        keyword = (KeywordAbility Deathtouch <$ ciString "Deathtouch")
              <|> (KeywordAbility Defender <$ ciString "Defender")
              <|> (KeywordAbility DoubleStrike <$ ciString "Double strike")
              <|> (KeywordAbility . Enchant . Target
                    (Exactly (AnyCount (NumValue 1)))
                    <$> (ciString "Enchant " *> targetMatch `sepBy1` orSep))
              <|> (KeywordAbility . Equip <$>
                    (ciString "Equip" *> keywordCostSep *> totalCost))
              <|> (KeywordAbility FirstStrike <$ ciString "First strike")
              <|> (KeywordAbility Flash <$ ciString "Flash")
              <|> (KeywordAbility Flying <$ ciString "Flying")
              <|> (KeywordAbility Haste <$ ciString "Haste")
              <|> (KeywordAbility Hexproof <$ ciString "Hexproof")
              <|> (KeywordAbility Indestructible <$ ciString "Indestructible")
              <|> (KeywordAbility Intimidate <$ ciString "Intimidate")
              -- TODO: Parse Landwalk
              <|> (KeywordAbility Lifelink <$ ciString "Lifelink")
              -- TODO: Parse Protection
              <|> (KeywordAbility Reach <$ ciString "Reach")
              <|> (KeywordAbility Shroud <$ ciString "Shroud")
              <|> (KeywordAbility Trample <$ ciString "Trample")
              <|> (KeywordAbility Vigilance <$ ciString "Vigilance")
              <|> (KeywordAbility Phasing <$ ciString "Phasing")
              <|> (ciString "Bestow" >> keywordCostSep >>
                    (KeywordAbility . Bestow) <$> totalCost)

        keywordCostSep = string " " <|> string "—"
