module Components.Settings where

import Control.Applicative (pure, (<$>))
--import Control.Bind (bind)
import Control.Bind (discard)
import Control.Category (identity)
import Control.Semigroupoid ((<<<))
--import Data.Eq (class Eq)
import Data.Function (($), const)
import Data.Maybe (Maybe(..))
--import Data.Ord (class Ord)
--import Data.Semiring ((+))
import Data.Show (show)
--import Data.Symbol (SProxy(..))
import Data.Unit (Unit, unit)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as Halogen
import Halogen.HTML as HTML
import Halogen.HTML.Events as HTML.Events
import Halogen.HTML.Properties as HTML.Properties

type    Surface     = HTML.HTML
data    Action      = NoAction
                    | Click
data    Query a     = GetSettings (Settings -> a)
type    Input       = Settings
data    Output      = UpdatedSettings Settings
                    | RegeneratePassword
--type    Output     = Settings
type    State       = Settings
type    Slot        = Halogen.Slot Query Output
type    Slots       = ()

type    Settings = { length :: Int }

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
render ({length:length}) = HTML.div [HTML.Properties.class_ (Halogen.ClassName "settings")] [
    HTML.h1  [] [HTML.text (show length)],
    HTML.button [HTML.Properties.title "new", HTML.Events.onClick \_ -> Just Click] [HTML.text "new"]
]

handleAction ∷ forall m. MonadAff m => Action → Halogen.HalogenM State Action Slots Output m Unit
handleAction = case _ of
    NoAction ->
        pure unit
    Click -> do
        Halogen.liftEffect $ log "Settings: click \"new\""
        Halogen.raise RegeneratePassword
    
--  SubComponentOutput (SubComponent.S_NoOutput) ->
--      pure unit
--  SubComponentOutput (SubComponent.S_Click_Happened) ->
--      Halogen.modify_ (\(State counter) -> State (counter + 1))

handleQuery :: forall m a. Query a -> Halogen.HalogenM State Action Slots Output m (Maybe a)
--handleQuery = const (pure Nothing)
handleQuery = case _ of
    GetSettings k -> do
        --result::Settings <- Halogen.get
        --pure (Just (k result))
        Just <<< k <$> Halogen.get

receive :: Input -> Maybe Action
receive = const Nothing

initialize :: Maybe Action
initialize = Just NoAction

finalize :: Maybe Action
finalize = Nothing