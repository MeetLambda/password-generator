module Components.Password where

import Control.Applicative (pure)
import Control.Bind (discard)
import Control.Category (identity)
import Control.Semigroupoid ((<<<))
-- import Data.Eq (class Eq)
import Data.Function (($), const)
import Data.Maybe (Maybe(..))
-- import Data.Ord (class Ord)
import Data.Semigroup ((<>))
-- import Data.Semiring ((+))
-- import Data.Show (show)
-- import Data.Symbol (SProxy(..))
import Data.Unit (Unit, unit)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as Halogen
import Halogen.HTML as HTML
import Halogen.HTML.Events as HTML.Events
import Halogen.HTML.Properties as HTML.Properties

type    Surface     = HTML.HTML
data    Action      = Click
                    | HandleInput String
data    Query a     = GetSettings a
type    Input       = String
data    Output      = RegeneratePassword
--                  | UpdatedPassword String
type    State      = String
type    Slot = Halogen.Slot Query Output
type    Slots = ()

initialState :: Input -> State
initialState = identity

component :: forall m. MonadAff m => Halogen.Component Surface Query Input Output m
component = Halogen.mkComponent {
    initialState,   -- :: Input -> State
    render,         -- :: State -> Surface (ComponentSlot Surface Slots m Action) Action
    eval: Halogen.mkEval $ Halogen.defaultEval {
        handleAction = handleAction,    --  handleAction    :: forall m. MonadAff m => Action → Halogen.HalogenM State Action Slots Output m Unit
        handleQuery  = handleQuery,     --  handleQuery     :: forall m a. Query a -> Halogen.HalogenM State Action Slots Output m (Maybe a)
        receive      = receive,         --  receive         :: Input -> Maybe Action
        initialize   = initialize,      --  initialize      :: Maybe Action
        finalize     = finalize         --  finalize        :: Maybe Action
    }
                    -- :: HalogenQ Query Action Input ~> HalogenM State Action Slots Output m
}

render :: forall m. {-MonadAff m =>-} State -> Halogen.ComponentHTML Action Slots m
render (password) = HTML.div [HTML.Properties.class_ (Halogen.ClassName "password")] [
    HTML.h1  [] [HTML.text ("<password>: " <> password)],
    HTML.button [HTML.Properties.title "new", HTML.Events.onClick \_ -> Just Click] [HTML.text "new"]
]

handleAction ∷ forall m. MonadAff m => Action → Halogen.HalogenM State Action Slots Output m Unit
handleAction = case _ of
    Click -> do
        Halogen.liftEffect $ log "Password: click \"new\""
        Halogen.raise RegeneratePassword
    HandleInput password -> do
        Halogen.put password


handleQuery :: forall m a. Query a -> Halogen.HalogenM State Action Slots Output m (Maybe a)
handleQuery = const (pure Nothing)

receive :: Input -> Maybe Action
--receive = const Nothing
receive = Just <<< HandleInput

initialize :: Maybe Action
initialize = Nothing    --  DO NOT generate a password when starting up
--initialize = Just Click --  Automatically generate a password when loading the component

finalize :: Maybe Action
finalize = Nothing