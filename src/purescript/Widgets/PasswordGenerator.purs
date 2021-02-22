module Widgets.PasswordGenerator where
  
import Concur.Core (Widget)
import Concur.Core.Gen (runWidget)
import Concur.React (HTML)
import Concur.React.DOM (div, text, h2, h4, a, p, span, button, form, label, input, fieldset, legend)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Apply ((<*>))
import Control.Bind (bind, discard, (=<<), (>>=))
import Control.Monad (class Monad)
import Control.Semigroupoid ((<<<), (>>>))
import Data.Boolean (otherwise)
import Data.Either (Either(..), note)
import Data.Function (identity, ($))
import Data.Functor (map, ($>), (<$>))
import Data.Int (fromString)
import Data.Lens.Lens (Lens'(..), lens)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (mempty)
import Data.Newtype (class Newtype)
import Data.Ord ((<), (>))
import Data.Semigroup ((<>))
import Data.Set (member)
import Data.Show (show)
import Data.String.CodeUnits (length)
import Data.String.Common (null)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console as Effect.Console
import Effect.Fortuna (randomBytes)
import Effect.Fortuna as PRNG
import Formless as Formless
import React.DOM.Dynamic (s)
import React.SyntheticEvent (SyntheticEvent_)
import Types.Settings (Password(..), Settings, charactersWithOptions)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Document (characterSet)


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
type FormValues = {
    length              :: Int,
    uppercaseLetters    :: Boolean,
    numbers             :: Boolean,
    lowercaseLetters    :: Boolean,
    spaces              :: Boolean,
    weirdchars          :: Boolean,
    characters          :: String
}

toFormValues :: Settings -> FormValues
toFormValues s = {
    length:             s.length,
    uppercaseLetters:   s.options.uppercaseLetters,
    lowercaseLetters:   s.options.lowercaseLetters,
    numbers:            s.options.numbers,
    spaces:             s.options.spaces,
    weirdchars:         s.options.weirdchars,
    characters:         s.characters
}

fromFormValues :: FormValues -> Settings
fromFormValues v = {
    length: v.length,
    options: {
        uppercaseLetters:   v.uppercaseLetters,
        numbers:            v.numbers,
        lowercaseLetters:   v.lowercaseLetters,
        spaces:             v.spaces,
        weirdchars:         v.weirdchars
    },
    characters:             v.characters
}

newtype SettingsForm r f = SettingsForm (r (
    --                       err   in       out
    length              :: f Error String   Int,
    uppercaseLetters    :: f Error Boolean  Boolean,
    numbers             :: f Error Boolean  Boolean,
    lowercaseLetters    :: f Error Boolean  Boolean,
    spaces              :: f Error Boolean  Boolean,
    weirdchars          :: f Error Boolean  Boolean,
    characters          :: f Error String   String
))
derive instance newtypeSettingsForm :: Newtype (SettingsForm r f) _

type SettingsInputForm  = SettingsForm Record Formless.InputField
type SettingsOutputForm = SettingsForm Record Formless.OutputField
type SettingsValidators = SettingsForm Record (Formless.Validation SettingsForm (Widget HTML))
type SettingsFormState  = Formless.State SettingsForm (Widget HTML)

formValues :: Settings -> SettingsInputForm
formValues defaultSettings = Formless.wrapInputFields {
    length:             show values.length,
    uppercaseLetters:   values.uppercaseLetters,
    lowercaseLetters:   values.lowercaseLetters,
    numbers:            values.numbers,
    spaces:             values.spaces,
    weirdchars:         values.weirdchars,
    characters:         values.characters
}
    where values = toFormValues defaultSettings

data Error
  = Required
  | NotNumber
  | TooShort | TooLong

toText :: Error -> String
toText Required     = "This field is required."
toText NotNumber    = "Not a number."
toText TooShort     = "This length is too short."
toText TooLong      = "This length is too long."
  
isNonEmpty :: ∀ form m. Monad m => Formless.Validation form m Error String String
isNonEmpty = Formless.hoistFnE_ $ \str -> if null str then Left Required else Right str

isNumber :: ∀ form m. Monad m => Formless.Validation form m Error String Int 
isNumber = Formless.hoistFnE_ $ (note NotNumber) <<< fromString

inRange :: ∀ form m. Monad m => Int -> Int -> Formless.Validation form m Error Int Int
inRange l t = Formless.hoistFnE_ $ \x -> if (x < l) then Left TooShort     else if (x > t) then Left TooLong     else Right x

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

stringLengthGreaterThan :: ∀ form m. Monad m => Int -> Formless.Validation form m Error String String
stringLengthGreaterThan l = Formless.hoistFnE_ $ checkLength
    where
        checkLength v = if ((length v) < l) then Left TooShort else Right v

noopValidation :: ∀ form m a. Monad m => Formless.Validation form m Error a a
noopValidation = Formless.hoistFn_ identity

validators :: SettingsValidators
validators = SettingsForm {
    length:             isNumber >>> inRange 6 99,
    -- length:             inRange 6 99,
    uppercaseLetters:   noopValidation,
    lowercaseLetters:   noopValidation,
    numbers:            noopValidation,
    spaces:             noopValidation,
    weirdchars:         noopValidation,
    -- characters:         isNonEmpty >>> (stringLengthInRange 4 10)
    characters:         isNonEmpty >>> (stringLengthGreaterThan 6)
}


unsafeTargetChecked ::
    forall r.
    SyntheticEvent_ r ->
    Boolean
unsafeTargetChecked e = (unsafeCoerce e).target.checked

settingsWidget :: Settings -> Widget HTML Settings
settingsWidget settings = go (Formless.initFormState (formValues settings) validators)
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
                            Props._type "checkbox",
                            Props.checked $ Formless.getInput _uppercaseLetters fstate.form,
                            (Formless.setValidate _uppercaseLetters <<< unsafeTargetChecked) <$> Props.onChange
                        ],
                        text "uppercase letters",
                        errorDisplay $ Formless.getError _uppercaseLetters fstate.form
                    ],

                    div [] [
                        input [
                            Props._type "checkbox",
                            Props.checked $ Formless.getInput _lowercaseLetters fstate.form,
                            (Formless.setValidate _lowercaseLetters <<< unsafeTargetChecked) <$> Props.onChange
                        ],
                        text "lowercase letters",
                        errorDisplay $ Formless.getError _lowercaseLetters fstate.form
                    ],
                    div [] [
                        input [
                            Props._type "checkbox",
                            Props.checked $ Formless.getInput _numbers fstate.form,
                            (Formless.setValidate _numbers <<< unsafeTargetChecked) <$> Props.onChange
                        ],
                        text "numbers",
                        errorDisplay $ Formless.getError _numbers fstate.form
                    ],
                    div [] [
                        input [
                            Props._type "checkbox",
                            Props.checked $ Formless.getInput _spaces fstate.form,
                            (Formless.setValidate _spaces <<< unsafeTargetChecked) <$> Props.onChange
                        ],
                        text "spaces",
                        errorDisplay $ Formless.getError _spaces fstate.form
                    ],
                    div [] [
                        input [
                            Props._type "checkbox",
                            Props.checked $ Formless.getInput _weirdchars fstate.form,
                            (Formless.setValidate _weirdchars <<< unsafeTargetChecked) <$> Props.onChange
                        ],
                        text "weird chars",
                        errorDisplay $ Formless.getError _weirdchars fstate.form
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
                    let values = fromFormValues $ Formless.unwrapOutputFields form :: Settings
                    liftEffect (Effect.Console.log $ "SUBMIT - return values")
                    pure values

            where
                _length             = SProxy :: SProxy "length"
                _uppercaseLetters   = SProxy :: SProxy "uppercaseLetters"
                _lowercaseLetters   = SProxy :: SProxy "lowercaseLetters"
                _numbers            = SProxy :: SProxy "numbers"
                _spaces             = SProxy :: SProxy "spaces"
                _weirdchars         = SProxy :: SProxy "weirdchars"
                _characters         = SProxy :: SProxy "characters"

                errorDisplay = maybe mempty (\err -> div [Props.style {color: "red"}] [text $ toText err])


suggestionWidget :: Password -> Widget HTML PasswordEvent
suggestionWidget password@(Password passwordValue) = do
    div [Props.className "password"] [
        input [Props._type "text", Props.value passwordValue],
        button [Props.onClick]  [text "New suggestion"] $> RegeneratePassword,
        button [Props.onClick]  [text "return"] $> (ReturnPassword password)
    ]

data PasswordEvent = RegeneratePassword | ReturnPassword Password
data Event = UpdateSettings Settings | UpdatePassword PasswordEvent

widget :: Settings -> Widget HTML Password
widget settings = do
    let password = Password "pippo" :: Password
    -- password :: Password <- PRNG.generatePassword (settingsFromFormState formState)
    --  PRNG :: Int -> Effect Bytes
    --  generatePassword :: (Length, Chars) -> PRNG -> password
    
    event :: Event <- (map UpdateSettings (settingsWidget settings)) <|> (map UpdatePassword (suggestionWidget password))
    case event of
        UpdateSettings settings' -> widget settings'
        UpdatePassword passwordEvent -> case passwordEvent of
            RegeneratePassword -> widget settings
            ReturnPassword password' -> pure password'

