module Widgets.Main where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (text, h4)
import Control.Bind (bind, discard)
import Data.Semigroup ((<>))
import Data.Set (singleton)
import Data.Show (show)
import Types.Settings (Settings, uppercaseLettersSet, Password(..))
import Widgets.PasswordGenerator as Widgets.PasswordGenerator
import Effect.Class (liftEffect)
import Effect.Console (log)

-- defaultSettings = { length:24, characterSets:singleton uppercaseLettersSet, characters:"luigi" } :: Settings
defaultSettings = {
    length              : 24,
    options: {
        uppercaseLetters    : true,
        numbers             : true,
        lowercaseLetters    : true,
        spaces              : false,
        weirdchars          : false
    },
    characters          : "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
} :: Settings

widget :: forall a. Widget HTML a
-- widget = h4 [] [text "Password Generator"]
widget = go defaultSettings
    where
        go :: Settings -> Widget HTML a
        go settings = do
            password :: Password <- Widgets.PasswordGenerator.widget defaultSettings
            liftEffect (log ("PASSWORD: " <> (show password)))
            go settings
            
