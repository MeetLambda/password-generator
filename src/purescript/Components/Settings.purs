module Components.Settings where

import Components.UIChunks as UIChunks
import Components.Validation as Validation
import Control.Applicative (pure, (<$>))
import Control.Bind (bind, discard)
import Control.Category (identity)
import Control.Monad (class Monad)
import Control.Semigroupoid ((<<<), (>>>))
import DOM.HTML.Indexed (HTMLbutton, HTMLinput)
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Boolean (otherwise)
import Data.Const (Const)
import Data.Either (Either(..), either)
import Data.Function (($), (#), const)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Ord ((<=), (>))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Formless (FormFieldResult(..))
import Formless as Formless
import Formless.Validation (Validation(..), hoistFnE_)
import Halogen as Formless.Types.Component
import Halogen as Halogen
import Halogen.HTML as HTML
import Halogen.HTML.Events as HTML.Events
import Halogen.HTML.Properties as HTML.Properties

type    Surface     = HTML.HTML
data    Action      = NoAction
--                  | UpdateLength
                    | HandleForm State
                    | Click
--                  | HandleSettingsForm Settings
data    Query a     = GetSettings (Settings -> a)
type    Input       = Settings
data    Output      = UpdatedSettings Settings
                    | RegeneratePassword
--type    Output     = Settings
type    State       = Settings
type    Slot        = Halogen.Slot Query Output

--  Slots :: (Type -> Type) -> Type -> Type -> Type
type    Slots       = (
--    form :: Formless.Slot (Const Void) Unit
)

data    CharacterSet = CapitalLetters | LowercaseLetters | Digits | Space | Symbols
type    Settings = {
    length :: Int
--  characterSets :: Set CharacterSet,
--  characters :: String
}

-- ###########################################################################################

newtype Form r f = Form (r (FormData f))
derive instance newtypeForm :: Newtype (Form r f) _

type FormData f = (
    -- name    :: f Validation.FieldError   String  String,
    -- email   :: f Validation.FieldError   String  Validation.Email,
    length :: f Validation.FieldError String Int
)

--  form
--            :: Formless.Component    Component HTML (Query form query slots) input msg m
formComponent :: forall m. MonadAff m => Formless.Component Form (Const Void) () Unit State m
formComponent = Formless.component (const formInput) $ Formless.defaultSpec { render = renderForm, handleEvent = Formless.raiseResult }
    where
    formInput = {
        validators: Form {
            -- name:    Validation.minLength 5,
            -- email:   Validation.emailFormat >>> Validation.emailIsUsed,
            length: Validation.strIsInt >>> Validation.enoughMoney
        },
        initialInputs: Nothing
    }

    renderForm { form } = UIChunks.formContent_ [
        UIChunks.input {
            label: "Password Length",
            help: Formless.getResult prx.length form # UIChunks.resultToHelp "How long do you want your password to be?",
            placeholder: "32"
        } [
            HTML.Properties.value $ Formless.getInput prx.length form,
            HTML.Events.onValueInput $ Just <<< Formless.asyncSetValidate (Milliseconds 500.0) prx.length
        ],
        UIChunks.buttonPrimary
            [ HTML.Events.onClick \_ -> Just Formless.submit ]
            [ HTML.text "Submit" ]
    ]
        where
        prx = Formless.mkSProxies (Formless.FormProxy :: _ Form)

-- ###########################################################################################

initialState :: Input -> State
initialState = identity

component :: forall m. MonadAff m => Halogen.Component Surface Query Input Output m
component = Halogen.mkComponent {
    initialState:   initialState,   -- :: Input -> State
    render:         render,         -- :: State -> Surface (ComponentSlot Surface Slots m Action) Action
    eval: Halogen.mkEval $ Halogen.defaultEval {
        handleAction = handleAction,    --  handleAction    :: forall m. MonadAff m => Action → Halogen.HalogenM State Action Slots Output m Unit
        handleQuery  = handleQuery,     --  handleQuery     :: forall m a. Query a -> Halogen.HalogenM State Action Slots Output m (Maybe a)
        receive      = receive,         --  receive         :: Input -> Maybe Action
        initialize   = initialize,      --  initialize      :: Maybe Action
        finalize     = finalize         --  finalize        :: Maybe Action
    }
                    -- :: HalogenQ Query Action Input ~> HalogenM State Action Slots Output m
}

--render :: forall m. MonadAff m => State -> Halogen.ComponentHTML Action Slots m
render :: ∀ i p. HTML.HTML i p
render ({length:length}) = HTML.div [HTML.Properties.class_ (Halogen.ClassName "settings")] [
    HTML.h1  [] [HTML.text (show length)],
--  HTML.input [HTML.Properties.name "length", HTML.Events.onChange updateLength, HTML.Events.onKeyDown updateLength, Halogen.HTML.Events.onKeyUp updateLength],
    HTML.slot Formless._formless unit formComponent unit (Just <<< HandleForm),
    HTML.button [HTML.Properties.title "new", HTML.Events.onClick \_ -> Just Click] [HTML.text "new"]
]

-- updateLength :: forall a. a -> Maybe Action
-- updateLength x = Just UpdateLength

handleAction :: forall m. MonadAff m => Action → Halogen.HalogenM State Action Slots Output m Unit
handleAction = case _ of
    NoAction ->
        pure unit
--  UpdateLength -> do
--      Halogen.raise RegeneratePassword
    Click -> do
        Halogen.liftEffect $ log "Settings: click \"new\""
        Halogen.raise RegeneratePassword
--  HandleSettingsForm settings -> do
--      Halogen.liftEffect $ log "SettingsFork: " <> logShow (settings :: Settings)
--      pure unit
    HandleForm s -> do
        Halogen.liftEffect $ log "HandleForm: " <> logShow (s :: State)
        -- pure unit

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