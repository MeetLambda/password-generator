module Components.Settings where

import Components.UIChunks as UIChunks
import Components.Validation (FieldError(..))
import Components.Validation as Validation
import Control.Applicative (pure, (<$>))
import Control.Bind (bind, discard)
import Control.Category (identity)
import Control.Monad (class Monad)
import Control.Monad.State.Class (class MonadState)
import Control.Semigroupoid ((<<<), (>>>))
import DOM.HTML.Indexed (HTMLbutton, HTMLinput)
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Boolean (otherwise)
import Data.Const (Const)
import Data.Either (Either(..), either)
import Data.Function (($), (#), const)
import Data.Functor (void)
import Data.Functor.Variant (VariantF)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Ord ((<=), (<), (>))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Symbol (SProxy(..))
import Data.Unit (Unit, unit)
import Data.Variant.Internal (FProxy)
import Data.Void (Void)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Formless (FormFieldResult(..))
import Formless as Formless
import Formless.Query (injQuery)
import Formless.Validation (Validation(..), hoistFnE_)
import Halogen as Formless.Types.Component
import Halogen as Halogen
import Halogen.Component (ComponentSlot)
import Halogen.HTML as HTML
import Halogen.HTML.Events as HTML.Events
import Halogen.HTML.Properties as HTML.Properties

type    Surface     = HTML.HTML
data    Action      = NoAction
                    | HandleForm State
                    | Click
data    Query a     = GetSettings (Settings -> a)
type    Input       = Settings
data    Output      = UpdatedSettings Settings
                    | RegeneratePassword
type    State       = Settings
type    Slot        = Halogen.Slot Query Output

type    ChildSlots  = ()
type    MessyType_1 = (Const Void)
type    MessyType_2 = Unit

-- type Slots = ( formless ∷ Slot (VariantF ( query :: FProxy (Formless.QueryF Form ()) , userQuery :: FProxy (Const Void) ) ) State Unit )
-- type Slots = ( formless ∷ Slot (VariantF ( query :: FProxy (Formless.QueryF Form ()) , userQuery :: FProxy (Const Void) ) ) State Unit )

data    CharacterSet = CapitalLetters | LowercaseLetters | Digits | Space | Symbols
type    Settings = {
    length :: Int
--  characterSets :: Set CharacterSet,
--  characters :: String
}

-- ###########################################################################################

type FormData f = (
    -- name    :: f Validation.FieldError   String  String,
    -- email   :: f Validation.FieldError   String  Validation.Email,
    length :: f Validation.FieldError String Int
)

newtype Form r f = Form (r (FormData f))
derive instance newtypeForm :: Newtype (Form r f) _

formInput :: forall m. Monad m => Formless.Input' Form m
formInput = {
    initialInputs: Nothing,
    validators: Form {
        length: Formless.hoistFnE_ \str -> case Int.fromString str of
            Nothing -> Left (InvalidInt str)
            Just n
                | n < 0  -> Left (TooShort n)
                | n > 30 -> Left (TooLong n)
                | otherwise -> Right n
    }
}

--            :: Formless.Component    Component HTML (Query form query slots) input msg m
formComponent :: forall m. MonadAff m => Formless.Component Form MessyType_1 ChildSlots MessyType_2 State m
formComponent = Formless.component (const formInput) $ Formless.defaultSpec {
        handleEvent = Formless.raiseResult,
        render = renderForm
    }
    where
        renderForm { form } = UIChunks.formContent_ [
            UIChunks.input {
                label: "Password Length",
                help: Formless.getResult _length form # UIChunks.resultToHelp "How long do you want your password to be?",
                placeholder: "32"
            } [
                HTML.Properties.value $ Formless.getInput _length form,
                HTML.Events.onValueInput $ Just <<< Formless.asyncSetValidate (Milliseconds 500.0) _length
            ],
            UIChunks.buttonPrimary
                [ HTML.Events.onClick \_ -> Just Formless.submit ]
                [ HTML.text "Submit" ]
        ]
            where
            _length = SProxy :: SProxy "length"

-- ###########################################################################################

initialState :: Input -> State
initialState = identity

-- mkComponent :: ∀ surface state query action slots input output m. ComponentSpec surface state query action slots input output m → Component surface query input output m
-- mkComponent :: ∀ surface state query action slots input output m. { eval ∷ forall a. HalogenQ query action input a -> HalogenM state action slots output m a , initialState ∷ input -> state , render ∷ state -> surface (ComponentSlot surface slots m action) action } → Component surface query input output m

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
}

-- render :: ∀ m. MonadAff m ⇒ State → Surface (ComponentSlot Surface Slots m Action ) Action
render ({length:length}) = HTML.div [HTML.Properties.class_ (Halogen.ClassName "settings")] [
    HTML.h1  [] [HTML.text (show length)],
    HTML.slot Formless._formless unit formComponent unit (Just <<< HandleForm),
    HTML.button [HTML.Properties.title "new", HTML.Events.onClick \_ -> Just Click] [HTML.text "new"]
]

-- updateLength :: forall a. a -> Maybe Action
-- updateLength x = Just UpdateLength

-- handleAction :: forall m. MonadAff m => Action -> Halogen.HalogenM State Action Slots Output m Unit
handleAction :: forall m t35. MonadAff m => Action -> Halogen.HalogenM State Action t35 Output m Unit
handleAction = case _ of
    NoAction -> do
      pure unit
    Click -> do
        Halogen.liftEffect $ log "Settings: click \"new\""
        Halogen.raise RegeneratePassword
    HandleForm s -> do
        Halogen.liftEffect $ log "HandleForm: " <> logShow (s :: State)


handleQuery :: forall m a. MonadState State m => Query a -> m (Maybe a)
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