module Widgets.PasswordGenerator where
  
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text, h2, h4, a, p, span, button)
import Concur.React.Props as Props
import Control.Bind (bind)
import Data.Function (($))
import Data.Functor (map, ($>))
import Data.Semigroup ((<>))
import Data.Show (show)
import Types.Settings (Password(..), Settings(..), suggestPassword)
import Effect.Aff.Class (liftAff)
import Effect.Fortuna (randomBytes)

settingsWidget :: Settings -> Widget HTML Settings
settingsWidget settings = button [Props.onClick]    [text "Settings"]    $> (settings)

suggestionWidget :: Settings -> Widget HTML Password
suggestionWidget settings@(Settings settingsRecord) = do
    bytes <- liftAff $ randomBytes settingsRecord.length
    button [Props.onClick]    [text "Suggestion"]    $> (suggestPassword settings bytes)

widget :: Settings -> Widget HTML Password
widget settings@(Settings settingsRecord) =
    button [Props.onClick] [text ("Password Generator " <> (show settingsRecord.length))] $> (Password "ciao")