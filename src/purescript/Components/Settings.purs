module Components.Settings where

import Control.Applicative (pure, (<$>))
import Control.Bind (discard)
import Control.Category (identity)
import Control.Monad (class Monad)
import Control.Semigroupoid ((<<<))
import Data.Boolean (otherwise)
import Data.Either (Either(..))
import Data.Function (($), const)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Ord ((<=))
import Data.Set (Set)
import Data.Show (show)
import Data.Unit (Unit, unit)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)

import Data.Int as Int
import Data.Set as Set
import Halogen as Halogen
import Halogen.HTML as HTML
import Halogen.HTML.Events as HTML.Events
import Halogen.HTML.Properties as HTML.Properties
import Formless as Formless

type    Surface     = HTML.HTML
data    Action      = NoAction
--                  | UpdateLength
                    | Click
data    Query a     = GetSettings (Settings -> a)
type    Input       = Settings
data    Output      = UpdatedSettings Settings
                    | RegeneratePassword
--type    Output     = Settings
type    State       = Settings
type    Slot        = Halogen.Slot Query Output
type    Slots       = ()

data    CharacterSet = CapitalLetters | LowercaseLetters | Digits | Space | Symbols
type    Settings = {
    length :: Int,
--  characterSets :: Set CharacterSet
    characters :: String
}

data NoLengthSpecified =    NoLengthSpecified | InvalidInt
data NoCharacterSelected =  NoCharacterSelected

newtype SettingsForm r f = SettingsForm ( r (
--  field               error                   input   output
    length      :: f    NoLengthSpecified       String  Int,
    characters  :: f    NoCharacterSelected     String  (Set Char)
))
derive instance newtypeSettingsForm :: Newtype (SettingsForm r f) _

settingsForm :: forall m. Monad m => Formless.Input' SettingsForm m
settingsForm = {
    initialInputs: Nothing, -- same as: Just (F.wrapInputFields { name: "", age: "" }),
    validators: SettingsForm {
        length: Formless.hoistFnE_ \str -> case Int.fromString str of
            Nothing -> Left InvalidInt
            Just n
                | n <= 0 -> Left NoLengthSpecified
                | otherwise -> Right (n)
        ,
        characters: Formless.hoistFnE_ \str -> Right (Set.fromFoldable str)
    }
}

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
--  HTML.input [HTML.Properties.name "length", HTML.Events.onChange updateLength, HTML.Events.onKeyDown updateLength, Halogen.HTML.Events.onKeyUp updateLength],
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