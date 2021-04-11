module Widgets.Main where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (text, h4)
import Control.Bind (bind, discard)
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Set (singleton)
import Data.Show (show)
import Widgets.PasswordGenerator as Widgets.PasswordGenerator
import Effect.Class (liftEffect)
import Effect.Console (log)

-- defaultSettings = { length:24, characterSets:singleton uppercaseLettersSet, characters:"luigi" } :: Settings
defaultSettings = {
    length              : 24,
    characters          : "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
} :: Widgets.PasswordGenerator.Settings

widget :: forall a. Widget HTML a
widget = do
    -- password :: Widgets.PasswordGenerator.Password <- Widgets.PasswordGenerator.widget defaultSettings (Widgets.PasswordGenerator.Loading Nothing (Widgets.PasswordGenerator.randomPassword defaultSettings.length defaultSettings.characters))
    let passwordComputation = Widgets.PasswordGenerator.randomPassword defaultSettings.length defaultSettings.characters
    -- let defaultPasswordValue = Widgets.PasswordGenerator.Loading Nothing
    let defaultPasswordValue = Widgets.PasswordGenerator.Loading (Just (Widgets.PasswordGenerator.Password "- - -"))
    password :: Widgets.PasswordGenerator.Password <- Widgets.PasswordGenerator.widget defaultSettings (Widgets.PasswordGenerator.AsyncValueWithComputation passwordComputation defaultPasswordValue)
    liftEffect (log ("PASSWORD: " <> (show password)))
    widget
            
