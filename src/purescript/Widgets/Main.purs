module Widgets.Main where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (text, h4)
import Control.Bind (bind, discard)
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
-- widget = h4 [] [text "Password Generator"]
widget = go defaultSettings
    where
        go :: Widgets.PasswordGenerator.Settings -> Widget HTML a
        go settings = do
            password :: Widgets.PasswordGenerator.Password <- Widgets.PasswordGenerator.widget defaultSettings
            liftEffect (log ("PASSWORD: " <> (show password)))
            go settings
            
