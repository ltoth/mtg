{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Game.MtG.Client.Console
  ( consoleChoiceFn
  ) where

import Control.Lens
import Control.Monad.Trans
import Data.Sequence.Lens
import Game.MtG.Types
import Text.Read (readMaybe)
import qualified IPPrint
import qualified Language.Haskell.HsColour as HsColour
import qualified Language.Haskell.HsColour.Colourise as HsColour
import qualified Language.Haskell.HsColour.Output as HsColour

-- |
-- = Console client

consoleChoiceFn :: MonadIO m => SPlayerChoice c -> KGame ->
                   PlayerChoiceRequest c -> m (PlayerChoiceResponse c)
consoleChoiceFn SChooseMulligan kg cs = do
  liftIO . myPrint $ cs
  putIO "Mulligan?"
  l <- liftIO getLine
  maybe
    invalid
    return
    (readMaybe l)
  where invalid = putIO "Invalid action" >>
                  consoleChoiceFn SChooseMulligan kg cs
consoleChoiceFn SChoosePriorityAction kg as =
  -- for debugging purposes, only "set a stop" at PreCombatMain
  if (kg^.step) /= PreCombatMain then
    return PassPriority
  else do
    liftIO . myPrint $ kg
    let sas = seqOf folded as
    printActions sas
    l <- liftIO getLine
    maybe
      invalid
      (\i -> case sas^?ix i of
               Just a  -> return a
               Nothing -> invalid)
      (readMaybe l)
  where printActions = imapM_ (\i a -> putIO $ show i ++ ": " ++ show a)
        invalid = putIO "Invalid action" >>
                  consoleChoiceFn SChoosePriorityAction kg as
-- TODO: implement
consoleChoiceFn SChooseModes kg ms = return []
-- TODO: implement
consoleChoiceFn SChooseManaAbilityActivation kg as = do
  let sas = seqOf folded as
  printActions sas
  l <- liftIO getLine
  maybe
    (return Nothing)
    (\i -> case sas^?ix i of
             Just a  -> return . Just $ a
             Nothing -> invalid)
    (readMaybe l)
  where printActions = imapM_ (\i a -> putIO $ show i ++ ": " ++ show a)
        invalid = putIO "Invalid action" >>
                  consoleChoiceFn SChooseManaAbilityActivation kg as

putIO :: MonadIO m => String -> m ()
putIO = liftIO . putStrLn

myColourPrefs :: HsColour.ColourPrefs
myColourPrefs = HsColour.defaultColourPrefs
  { HsColour.conid = [HsColour.Foreground HsColour.Magenta]
  , HsColour.conop = [HsColour.Foreground HsColour.Yellow]
  , HsColour.string = [HsColour.Foreground HsColour.Green]
  , HsColour.char = [HsColour.Foreground HsColour.Red]
  , HsColour.number = [HsColour.Foreground HsColour.Red]
  , HsColour.layout = [HsColour.Foreground HsColour.White]
  , HsColour.keyglyph = [HsColour.Foreground HsColour.White]
  }

myPrint :: Show a => a -> IO ()
myPrint = putStrLn . HsColour.hscolour
  (HsColour.TTYg HsColour.XTerm256Compatible) myColourPrefs
  False False "" False . IPPrint.pshow
