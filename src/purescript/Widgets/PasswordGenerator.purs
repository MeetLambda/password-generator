module Widgets.PasswordGenerator where
  
import Bytes (foldMapBytesToString)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text, button, input)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad (class Monad)
import Control.Monad.Rec.Class (forever)
import Control.Semigroupoid ((<<<), (>>>))
import Data.DateTime.Instant (toDateTime)
import Data.Either (Either(..), note, hush)
import Data.Formatter.DateTime (Formatter, format, parseFormatString)
import Data.Function (($), flip)
import Data.Functor (map, ($>), (<$>))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (mempty)
import Data.Newtype (class Newtype)
import Data.Ord ((<=), (<), (>))
import Data.Ring ((-))
import Data.Semigroup ((<>))
import Data.Semiring ((+))
import Data.Show (show, class Show)
import Data.String.CodePoints (length, take, drop)
import Data.String.Common (null)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console as Effect.Console
import Effect.Fortuna (randomBytes)
import Effect.Now (now)
import Formless as Formless

{-
                       +---------------------------------------------+
      wrapInputFields  |   +----------+          +---------------+   |
            +------------->|  input   |          |  validators   |   |------+
            |          |   +----------+          +---------------+   |      |
+-----------+----+     +---------------------------------------------+      |
|   record       |                                                          |
+-----------+----+                                                   initFromState
            ^              +------------+     eval                          |
            +--------------|  output    |<-----------+                      |
    unwrapOutputFields     +------------+            |                      |
                                                     |                      v
                              +----------------------+--------------------------+
                              |   +------------+     render    +------------+   |
                              |   |  query     |<--------------|  state     |   |
                              |   +------------+               +------------+   |
                              +-------------------------------------------------+
-}

--  https://github.com/ajnsit/purescript-formless-independent

-- ===================================================================================

data Error
  = Required
  | NotNumber
  | TooShort | TooLong

-- ===================================================================================

type Settings = {
    length              :: Int,
    characters          :: String
}

newtype SettingsForm :: (Row Type -> Type) -> (Type -> Type -> Type -> Type) -> Type
newtype SettingsForm r f = SettingsForm (r (
    length              :: f Error String   Int,
    characters          :: f Error String   String
))
derive instance newtypeSettingsForm :: Newtype (SettingsForm r f) _

type SettingsInputForm  = SettingsForm Record Formless.InputField
type SettingsOutputForm = SettingsForm Record Formless.OutputField
type SettingsValidators = SettingsForm Record (Formless.Validation SettingsForm (Widget HTML))
type SettingsFormState  = Formless.State SettingsForm (Widget HTML)

formValues :: Settings -> SettingsInputForm
formValues values = Formless.wrapInputFields {
    length:             show values.length,
    characters:         values.characters
}


-- ===================================================================================
{-
toText :: Error -> String
toText Required     = "This field is required."
toText NotNumber    = "Not a number."
toText TooShort     = "This length is too short."
toText TooLong      = "This length is too long."
  
noValidation :: ∀ form m a. Monad m => Formless.Validation form m Error a a
noValidation = Formless.hoistFnE_ $ Right

broken :: ∀ form m a. Monad m => Error -> Formless.Validation form m Error a a
broken e = Formless.hoistFnE_ $ \_ -> Left e

mustBeChecked :: ∀ form m. Monad m => Formless.Validation form m Error Boolean Boolean
mustBeChecked = Formless.hoistFnE_ $ \v -> if v then Right v else Left Required

stringToInt :: ∀ form m. Monad m => Formless.Validation form m Error String Int
stringToInt = Formless.hoistFnE_ $ fromString >>> (note NotNumber)

stringLengthInRange :: ∀ form m. Monad m => Int -> Int -> Formless.Validation form m Error String String
stringLengthInRange l t = Formless.hoistFnE_ $ checkLength
    where
        checkLength v = if ((length v) < l) then Left TooShort     else if (length v > t) then Left TooLong     else Right v

noopValidation :: ∀ form m a. Monad m => Formless.Validation form m Error a a
noopValidation = Formless.hoistFn_ identity

settingValidators :: SettingsValidators
settingValidators = SettingsForm {
    length:             isNumber >>> inRange 6 99,
    characters:         isNonEmpty >>> (stringLengthGreaterThan 6)
}

unsafeTargetChecked ::
    forall r.
    SyntheticEvent_ r ->
    Boolean
unsafeTargetChecked e = (unsafeCoerce e).target.checked

-}

isNumber :: ∀ form m. Monad m => Formless.Validation form m Error String Int 
isNumber = Formless.hoistFnE_ $ (note NotNumber) <<< fromString

inRange :: ∀ form m. Monad m => Int -> Int -> Formless.Validation form m Error Int Int
inRange l t = Formless.hoistFnE_ $ \x -> if (x < l) then Left TooShort     else if (x > t) then Left TooLong     else Right x

isNonEmpty :: ∀ form m. Monad m => Formless.Validation form m Error String String
isNonEmpty = Formless.hoistFnE_ $ \str -> if null str then Left Required else Right str

stringLengthGreaterThan :: ∀ form m. Monad m => Int -> Formless.Validation form m Error String String
stringLengthGreaterThan l = Formless.hoistFnE_ $ checkLength
    where
        checkLength v = if ((length v) < l) then Left TooShort else Right v


toText :: Error -> String
toText _     = "ERRORE"

settingValidators :: SettingsValidators
settingValidators = SettingsForm {
    length:             isNumber >>> inRange 6 99,
    characters:         isNonEmpty >>> (stringLengthGreaterThan 6)
}

-- ===================================================================

data Password = Password String
instance showPassword :: Show Password where
    show (Password p) = "password[" <> p <> "]"

randomPassword :: Int -> String -> Aff Password
randomPassword l characters = map Password $ appendRandomChars (repeatStringUpToSize 256 characters) l ""
    where
        appendRandomChars :: String -> Int -> String -> Aff String
        appendRandomChars chars n p | n <= length p = pure (take n p)
        appendRandomChars chars n p = do
            bytes <- randomBytes (n - (length p))
            appendRandomChars chars n (p <> (foldMapBytesToString (characterAtIndex chars) bytes))

        characterAtIndex :: String -> Int -> String
        characterAtIndex s 0 = take 1 s
        characterAtIndex s i | i < length s = take 1 (drop i s)
        characterAtIndex s i = ""

        repeatStringUpToSize :: Int -> String -> String
        repeatStringUpToSize n s = repeatStringUpToSize' n s s
            where
                repeatStringUpToSize' :: Int -> String -> String -> String
                -- repeatStringUpToSize' n "" a = ""
                -- repeatStringUpToSize' n s  a | (length a) + (length s) <= n = repeatStringUpToSize' n s (a <> s)
                -- repeatStringUpToSize' n s  a = a
                repeatStringUpToSize' _  ""  _ = ""
                repeatStringUpToSize' n' s'  a | (length a) + (length s') <= n' = repeatStringUpToSize' n' s' (a <> s')
                repeatStringUpToSize' _  _   a = a


-- ===================================================================

settingsWidget :: Settings -> Widget HTML Settings
settingsWidget settings = go (Formless.initFormState (formValues settings) settingValidators)
    where
        go fstate = do
            query :: Formless.Query SettingsForm <-
                div [Props.className "settings"] [
                    div [] [
                        input [
                            Props._type "number",
                            Props.value $ Formless.getInput _length fstate.form,
                            (Formless.setValidate _length <<< Props.unsafeTargetValue) <$> Props.onChange
                        ],
                        errorDisplay $ Formless.getError _length fstate.form
                    ],
                    div [] [
                        input [
                            Props.value $ Formless.getInput _characters fstate.form,
                            Props.size 60,
                            (Formless.setValidate _characters <<< Props.unsafeTargetValue) <$> Props.onChange
                        ],
                        errorDisplay $ Formless.getError _characters fstate.form
                    ]
                ]
            result <- Formless.eval (Formless.andThen query Formless.submit) fstate
            case result of
                Left state -> do
                    liftEffect (Effect.Console.log $ "SETTINGS NOT VALID - keep handling form")
                    go state
                Right form  -> do
                    let values = Formless.unwrapOutputFields form :: Settings
                    liftEffect (Effect.Console.log $ "SUBMIT - return values: " <> (show values))
                    pure values

            where
                _length             = SProxy :: SProxy "length"
                _characters         = SProxy :: SProxy "characters"

                errorDisplay = maybe mempty (\err -> div [Props.style {color: "red"}] [text $ toText err])


passwordValue :: Password -> String
passwordValue (Password password) = password

-- ===============================================================

suggestionWidget :: AsyncValue Password -> Widget HTML (Either (AsyncValue Password) Password)
suggestionWidget (Loading placeholder) = do
    liftEffect (Effect.Console.log $ "suggestionWidget - LOADING: " <> show placeholder)
    div [Props.className "password loading"] [
        input [Props._type "text", Props.disabled true, Props.defaultValue placeholderValue],
        button [Props.disabled true]  [text "New suggestion"],
        button [Props.disabled true]  [text "return"]
    ]
    where
        placeholderValue :: String
        placeholderValue = fromMaybe "---" (map passwordValue placeholder)
suggestionWidget (Done password) = do
    liftEffect (Effect.Console.log $ "suggestionWidget - DONE: " <> passwordValue password)
    div [Props.className "password"] [
        input [Props._type "text", Props.value $ passwordValue password],
        button [Props.onClick]  [text "New suggestion"] $> (Left (Loading Nothing)),
        button [Props.onClick]  [text "return"] $> (Right password)
    ]

-- ============================================================================

data AsyncValue a = Loading (Maybe a) | Done a
instance showAsyncValue :: (Show a) => Show (AsyncValue a) where
    show (Loading p) = "loading[" <> show p <> "]"
    show (Done p) = "done[" <> show p <> "]"

-- ============================================================================

--  Async execution
--
--  - https://blog.drewolson.org/asynchronous-purescript
--  - https://github.com/JordanMartinez/purescript-jordans-reference/tree/latestRelease/21-Hello-World/02-Effect-and-Aff

-- ============================================================================

clockWidget :: forall a. Widget HTML a
clockWidget = forever do
    renderClock <|> liftAff (delay (Milliseconds 1000.0))
    where
        renderClock :: forall a'. Widget HTML a'
        renderClock = do
            time <- liftEffect currentTime
            div [Props.className "clock"] [text time]

        currentTime :: Effect String
        currentTime = do
            t <- now
            let nowDateTime = toDateTime t
            let formatter = hush $ parseFormatString "HH:mm:ss" :: Maybe Formatter
            pure $ maybe "---" (flip format nowDateTime) formatter

-- ============================================================================
