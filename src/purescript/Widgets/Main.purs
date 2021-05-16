module Widgets.Main where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (text, div)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Semigroupoid ((<<<))
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor (map, (<#>))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (show)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Widgets.PasswordGenerator (AsyncValue(..), Password, Settings, randomPassword, settingsWidget, suggestionWidget)

defaultSettings = {
    length              : 24,
    characters          : "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
} :: Settings

-- ===================================================================

data ComposedWidgetResult = UpdateSettings Settings | PasswordResult (Either (AsyncValue Password) Password)
-- instance showComposedWidgetResult :: Show ComposedWidgetResult where
--     show (UpdateSettings s) = "UpdateSettings: " <> show s
--     show (PasswordResult v) = "PasswordResult: " <> show v

composedWidget :: Settings -> (AsyncValue Password) -> Widget HTML Password
composedWidget s av = do
    r :: ComposedWidgetResult <- case av of
        Done p      -> ui s av
        Loading mp  -> ui s av <|> (map (PasswordResult <<< Left) (computePassword s))
    -- liftEffect (log $ "R: " <> show r)
    case r of
        UpdateSettings s'           -> composedWidget s' (Loading Nothing)    --  if the password is set, should reset it (returning (Loading Nothing)) only if its value does not fit the new settings
        PasswordResult (Left  av')  -> composedWidget s av'
        PasswordResult (Right p)    -> pure p

    where
        ui :: Settings -> (AsyncValue Password) -> Widget HTML ComposedWidgetResult
        ui s' av' = div [Props.className "composedWidget"] [
            (settingsWidget s') <#> UpdateSettings,
            (suggestionWidget av') <#> PasswordResult
        ]

        computePassword :: Settings -> Widget HTML (AsyncValue Password)
        computePassword s' = liftAff $ map Done (randomPassword s'.length s'.characters)


widget :: forall a. Widget HTML a
widget = run defaultSettings defaultPassword
    where
        run :: forall a'. Settings -> AsyncValue Password -> Widget HTML a'
        run settings passwordAsyncValue = do -- Widget
            password <- composedWidget settings passwordAsyncValue
            liftEffect (log $ "DONE: " <> show password)
            text "DONE"

-- ========================================================================================

-- logSignal :: forall m. Monad m => Plus m => MonadEffect m => String -> SignalT m Unit
-- logSignal s = fireOnce_ (liftEffect (log s))

defaultPassword :: AsyncValue Password
defaultPassword = Loading Nothing
-- defaultPassword = Done (Password "pippo")

