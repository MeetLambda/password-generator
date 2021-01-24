module Widgets.Main where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (text, h4)
import Data.Set (singleton)
import Types.Settings (Settings(..), uppercaseLettersSet)
import Widgets.PasswordGenerator (widget as passwordWidget)

defaultSettings = Settings { length:8, characterSets:singleton uppercaseLettersSet, characters:"luigi" } :: Settings

widget :: forall a. Widget HTML a
-- widget = h4 [] [text "Password Generator"]
widget = passwordWidget defaultSettings