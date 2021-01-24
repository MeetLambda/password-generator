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
import Types.Settings (Password(..), Settings, suggestPassword)
import Effect.Aff.Class (liftAff)
import Effect.Fortuna (randomBytes)

settingsWidget :: Settings -> Widget HTML Settings
settingsWidget settings = button [Props.onClick]    [text "Settings"]    $> (settings)

suggestionWidget :: Settings -> Widget HTML Password
suggestionWidget settings = do
    bytes <- liftAff $ randomBytes settings.length
    button [Props.onClick]    [text "Suggestion"]    $> (suggestPassword settings bytes)

widget :: Settings -> Widget HTML Password
-- widget = h4 [] [text "Password Generator"] $> (Password "ciao")
widget settings = h4 [] [text ("Password Generator " <> (show settings.length))] $> (Password "ciao")