module Components.Main where

import Components.Password as Components.Password
import Components.Settings as Components.Settings
import Control.Applicative (pure)
import Control.Bind (bind, discard, class Bind)
import Control.Category (identity)
import Control.Monad.State.Class (class MonadState)
import Control.Semigroupoid ((<<<), (>>>))  --  (<<<) = Control.Semigroupoid.compose
                                            --  (>>>) = Control.Semigroupoid.composeFlipped
import Data.Eq (class Eq)
import Data.Function (($), const)   --  ($)   = Data.Function.apply
import Data.Maybe (Maybe(..), maybe)
import Data.Ord (class Ord)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String.Utils (repeat)
import Data.Symbol (SProxy(..))
import Data.Unit (Unit, unit)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Halogen as Halogen
import Halogen.HTML as HTML
import Halogen.HTML.Events as HTML.Events
import Halogen.HTML.Properties as HTML.Properties

-- import Halogen.Query.EventSource (finalize)

{-
- `surface` is the type that will be rendered by the component, usually `HTML`               :: Type -> Type -> Type
- `query` is the query algebra; the requests that can be made of the component               :: Type -> Type
- `input` is the input value that will be received when the parent of this component renders :: Type
- `output` is the type of messages the component can raise                                   :: Type
- `m` is the effect monad used during evaluation                                             :: Type -> Type
-}

type    Settings    = Components.Settings.Settings

type    Surface     = HTML.HTML

data    Action      = UpdateSettings        Components.Settings.Output
                    | UpdatePassword        Components.Password.Output
                    | GeneratePassword
                    | Click
data    Query a     = NoQuery a
type    Input       = Settings
data    Output      = NoOutput      -- aka Message
type    State       = {
    settings :: Settings,
    password :: String
}

--type    Slot = Halogen.Slot Query Output
type Slots = (
    settings :: Components.Settings.Slot Unit,
    password :: Components.Password.Slot Unit
)

_settings :: SProxy "settings"
_settings = SProxy

_password :: SProxy "password"
_password = SProxy

initialState :: Input -> State
initialState s = { settings: s, password: "" }

-- component :: forall m. {- MonadAff m => -} Halogen.Component Surface Query Input Output m
component :: forall m. MonadAff m => Halogen.Component Surface Query Input Output m
component = Halogen.mkComponent {
    initialState ,                                      -- :: Input -> State
    render,                                             -- :: State -> Surface (ComponentSlot Surface Slots m Action) Action
    eval: Halogen.mkEval $ Halogen.defaultEval {        -- :: HalogenQ query action input ~> HalogenM state action slots output m
        handleAction = handleAction :: forall m.   MonadAff m => Action  → Halogen.HalogenM State Action Slots Output m Unit,
        handleQuery  = handleQuery  :: forall m a. MonadAff m => Query a -> Halogen.HalogenM State Action Slots Output m (Maybe a),
        initialize   = initialize   :: Maybe Action,
        receive      = receive      :: Input -> Maybe Action,
        finalize     = finalize     :: Maybe Action
    }
}

render :: forall m. MonadAff m => State -> Halogen.ComponentHTML Action Slots m
render ({settings: settings, password: password}) = HTML.div [] [
    HTML.h1  [] [HTML.text "Hello!"],
    HTML.button [HTML.Properties.title "new", HTML.Events.onClick \_ -> Just Click] [HTML.text "new"],
    HTML.div [] [HTML.slot _settings unit Components.Settings.component (settings) (Just <<< UpdateSettings)],
    HTML.div [] [HTML.slot _password unit Components.Password.component (password) (Just <<< UpdatePassword)],
    --HTML.div [] [HTML.span [] [HTML.text "length:"],   HTML.span [] [HTML.text (show length)]],
    --HTML.div [] [HTML.span [] [HTML.text "password:"], HTML.span [] [HTML.text password]],
    HTML.hr_
]

-- handleAction ∷ forall m. {- MonadAff m => -} Action → Halogen.HalogenM State Action Slots Output m Unit
handleAction ∷ forall m. MonadAff m => Action → Halogen.HalogenM State Action Slots Output m Unit
handleAction = case _ of
    UpdateSettings (Components.Settings.UpdatedSettings settings) ->
        Halogen.modify_ (\state -> state { settings = settings })
    UpdateSettings (Components.Settings.RegeneratePassword) -> do
        Halogen.liftEffect $ log "handleAction \"UpdatePassword\" (Settings)"
        generatePasswordAction
    UpdatePassword (Components.Password.RegeneratePassword) -> do
        Halogen.liftEffect $ log "handleAction \"UpdatePassword\" (Password)"
        generatePasswordAction
    GeneratePassword -> do
        Halogen.liftEffect $ log "handleAction \"GeneratePassword\""
        generatePasswordAction
    Click -> do
        Halogen.liftEffect $ log "handleAction \"UpdatePassword\" (Main)"
        generatePasswordAction

generatePasswordAction :: ∀ m. Bind m ⇒ MonadEffect m ⇒ MonadState State m ⇒ m Unit
generatePasswordAction = do
    settings :: Settings <- Halogen.gets _.settings
    password :: String <- pure (generatePassword settings)
    Halogen.modify_ (\state -> state { password = "drowssap" })
--  password :: String <- Halogen.gets _.password
    Halogen.liftEffect $ log ("new password: " <> password)
  


handleQuery :: forall m a. Query a -> Halogen.HalogenM State Action Slots Output m (Maybe a)
handleQuery = const (pure Nothing)
--handleQuery = case _ of
--  NoQuery k -> do
--    pure (Just (k))

receive :: Input -> Maybe Action
receive = const Nothing

initialize :: Maybe Action
initialize = Nothing
--initialize = Just GeneratePassword

finalize :: Maybe Action
finalize = Nothing

-- ============================================================================

generatePassword :: Settings -> String
generatePassword { length: length } = "poppp" -- maybe "" identity (repeat length "*")