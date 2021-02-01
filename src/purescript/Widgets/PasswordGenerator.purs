module Widgets.PasswordGenerator where
  
import Concur.Core (Widget)
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
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Ord ((<), (>))
import Data.Semigroup ((<>))
import Data.Set (member)
import Data.Show (show)
import Data.String.Common (null)
import Effect.Aff.Class (liftAff)
import Effect.Fortuna (randomBytes)
import Formless.Transform.Record (wrapInputFields)
import Formless.Types.Form (InputField(..), OutputField)
import Formless.Types.Query (State)
import Formless.Validation (Validation, hoistFn_, hoistFnE_)
import React.DOM.Dynamic (s)
import React.SyntheticEvent (SyntheticEvent_)
import Types.Settings (Password(..), Settings(..), suggestPassword, uppercaseLettersSet, lowercaseLettersSet, numbersSet, spacesSet, weirdcharsSet)

-- type  CharSet = Set Char

-- uppercaseLettersSet = stringToSet uppercaseLetters                  :: CharSet
-- lowercaseLettersSet = stringToSet $ toLowerCase uppercaseLetters    :: CharSet
-- numbersSet          = stringToSet "0123456789"                      :: CharSet
-- spacesSet           = stringToSet " "                               :: CharSet
-- weirdcharsSet       = stringToSet "!#$%…"                           :: CharSet

-- type Settings = {
--     length :: Int,
--     characterSets :: Set CharSet,
--     characters :: String
-- }

--  https://github.com/ajnsit/purescript-formless-independent
newtype SettingsForm r f = SettingsForm (r (
    --                       err   in       out
    length              :: f Error Int      Int,
    uppercaseLetters    :: f Error Boolean  Boolean,
    numbers             :: f Error Boolean  Boolean,
    lowercaseLetters    :: f Error Boolean  Boolean,
    spaces              :: f Error Boolean  Boolean,
    weirdchars          :: f Error Boolean  Boolean,
    characters          :: f Error String   String
))
derive instance newtypeSettingsForm :: Newtype (SettingsForm r f) _

type SettingsInputForm  = SettingsForm Record InputField
type SettingsOutputForm = SettingsForm Record OutputField
type SettingsValidators = SettingsForm Record (Validation SettingsForm (Widget HTML))
type SettingsFormState  = State SettingsForm (Widget HTML)

inputs :: Settings -> SettingsInputForm
inputs settings = SettingsForm {
    length:             InputField settings.length,
    uppercaseLetters:   InputField (member uppercaseLettersSet  settings.characterSets),
    numbers:            InputField (member lowercaseLettersSet  settings.characterSets),
    lowercaseLetters:   InputField (member numbersSet           settings.characterSets),
    spaces:             InputField (member spacesSet            settings.characterSets),
    weirdchars:         InputField (member weirdcharsSet        settings.characterSets),
    characters:         InputField settings.characters
}

inputs' :: Settings -> SettingsInputForm
inputs' settings = wrapInputFields {
    length:             settings.length,
    uppercaseLetters:   (member uppercaseLettersSet  settings.characterSets),
    numbers:            (member lowercaseLettersSet  settings.characterSets),
    lowercaseLetters:   (member numbersSet           settings.characterSets),
    spaces:             (member spacesSet            settings.characterSets),
    weirdchars:         (member weirdcharsSet        settings.characterSets),
    characters:         settings.characters
}

data Error
  = Required
  | NotNumber
  | TooShort | TooLong
--   | NotEqual String String
--   | EmailIsUsed
--   | EmailInvalid

isNonEmpty :: ∀ form m. Monad m => Validation form m Error String String
isNonEmpty = hoistFnE_ $ \str ->
    if null str
        then Left Required
        else Right str

inRange :: ∀ form m. Monad m => Int -> Int -> Validation form m Error Int Int
inRange l t = hoistFnE_ $ \x ->
        if x < l
            then Left TooShort 
        else if x > t
            then Left TooLong
        else Right x

booleanNoop :: ∀ form m. Monad m => Validation form m Error Boolean Boolean
booleanNoop = hoistFnE_ $ \v -> Right v

validators :: SettingsValidators
validators = SettingsForm {
    -- name: isNonEmpty,
    -- email1: isNonEmpty >>> validEmail >>> emailNotUsed,
    -- email2: isNonEmpty >>> equalsEmail1 >>> emailNotUsed,

    length:             isNonEmpty >>> fromString >>> (note NotNumber) >>> (inRange 1 99),
    uppercaseLetters:   booleanNoop,
    numbers:            booleanNoop,
    lowercaseLetters:   booleanNoop,
    spaces:             booleanNoop,
    weirdchars:         booleanNoop,
    characters:         isNonEmpty
}



{-

-- For example, this validator simply transforms the input `Int` into a `String` using `hoistFn_`
-- output.
myStringValidator :: ∀ form m. Monad m => Validation form m Void Int String
myStringValidator = hoistFn_ show

-- This helper function lets you take any function from `input` to `Either error output` and turns
-- it into the Validation type from Formless.
hoistFnE_ :: ∀ form m e i o. Monad m => (i -> Either e o) -> Validation form m e i o

-- For example, this validator makes sure that the string is not empty
isNonEmpty :: ∀ form m. Monad m => Validation form m Error String String
isNonEmpty = hoistFnE_ $ \str ->
  if null str
     then Left Required
     else Right str

-- This validator transforms the input into an `Email` type if successful.
validEmail :: ∀ form m. Monad m => Validation form m Error String Email
validEmail = hoistFnE_ $ \str ->
  if contains (Pattern "@") str
     then Right (Email str)
     else Left EmailInvalid

-- Continuing the trend, this helper takes a function from `input` to a monad `m (Either error output)` and
-- turns it into the Validation type from Formless.
hoistFnME_ :: ∀ form m e i o. Monad m => (i -> m (Either e o)) -> Validation form m e i o

-- For example, this validator makes sure that an email address is not in use. Notice how it relies
-- on the input value already being an `Email` -- we'll see how to chain validators together so this
-- can be used with `validEmail` in a moment.
emailNotUsed :: ∀ form. Validation form Aff Error Email Email
emailNotUsed = hoistFnME_ $ \email -> do
  isUsed <- checkEmailIsUsed :: Email -> Aff Boolean
  pure $
    if isUsed
      then Right email
      else Left EmailIsUsed

-- Now, let's do something a little more complex. Let's validate that two fields are equal to one another.

-- This time, we want to rely on our existing `Form` as an argument for our validation, so instead of using
-- `hoistFnE_` we'll reach for `hoistFnE`, which doesn't throw away the form argument.
hoistFnE :: ∀ form m e i o. Monad m => (form Record FormField -> i -> Either e o) -> Validation form m e i o

-- We'll use `getInput` from Formless to retrieve the input value of the field "email1" from the form, and then
-- we'll validate that the current field is equal to it. Formless can prove that a "email1" field exists using
-- your form row, so you'll never access a value you don't have.
equalsEmail1 :: ∀ m. Monad m => Validation Form m Error String String
equalsEmail1 = hoistFnE $ \form str ->
  let e1 = F.getInput (SProxy :: SProxy "email1") form
   in if str == e1
        then Right str
        else Left $ NotEqual str e1


-}







-- data Data = S Settings | P Password

--  https://thomashoneyman.com/articles/practical-profunctor-lenses-optics/
--  https://github.com/ajnsit/purescript-formless-independent

settingsSetLength' :: Settings -> Int -> Settings
settingsSetLength' settings n =  settings { length = n }

-- lens' :: forall b a t s. (s -> Tuple a (b -> t)) -> Lens s t a b
-- lengthLens :: Lens' (Settings -> Int) (Settings -> Int -> Settings)
-- lengthLens :: Lens' Settings Int
-- lengthLens = lens (\(Settings values) -> values.length) (\(Settings values) n -> Settings (values { length = n }))

-- settingsWithInt :: Settings -> Int -> Settings
-- settingsWithInt s i = s

setter :: forall r a. Settings -> (String -> Maybe a) -> (Settings -> a -> Settings) -> SyntheticEvent_ r -> Settings
-- setter settings converter setter = ((fromMaybe settings) <<< (map (setter settings)) <<< converter <<< Props.unsafeTargetValue)
setter v c s = ((fromMaybe v) <<< (map (s v)) <<< c <<< Props.unsafeTargetValue)

{-
    form <=> record = [field]

    field Value => lens record
        Value -> Maybe Error -> Widget HTML Value
        validation rule => SyntheticEvent_ r -> Either Error Value
-}

log :: forall r. Settings -> SyntheticEvent_ r -> Settings
log settings event = settings { characters = Props.unsafeTargetValue event }

settingsWidget :: Settings -> Widget HTML Settings
-- settingsWidget settings = button [Props.onClick]    [text "Settings"]    $> (settings)
settingsWidget settings = div [Props.className "passwordSettings"] [
    form [] [
        label [] [
            text "length",
            -- input [((fromMaybe settings) <<< (map (settingsSetLength' settings)) <<< fromString <<< Props.unsafeTargetValue) <$> Props.onChange, Props.className "length", Props._type "number", Props.min "1", Props.max "99", Props.value (show settingsValue.length)]
            -- input [(setter settings fromString settingsSetLength') <$> Props.onChange, Props.className "length", Props._type "number", Props.min "1", Props.max "99", Props.value (show settingsValue.length)]
            input [log settings <$> Props.onChange, Props.className "length", Props._type "number", Props.min "1", Props.max "99", Props.value (show settings.length)]

            {-
                data Action = Changed String | Focused

                inputWidget :: Widget HTML Action
                inputWidget = input [(Changed <<< unsafeTargetValue) <$> onChange, Focused <$ onFocus]

            -}
        ],
        fieldset [] [
            legend [] [text "characters"],
            label [] [
                input [log settings <$> Props.onChange, Props._type "checkbox"],
                text "A-Z"
            ],
            label [] [
                input [Props._type "checkbox"],
                text "a-z"
            ],
            label [] [
                input [Props._type "checkbox"],
                text "0-9"
            ],
            label [] [
                input [Props._type "checkbox"],
                text "space"
            ],
            label [] [
                input [Props._type "checkbox"],
                text "!#?"
            ],
            input [log settings <$> Props.onChange, Props._type "text", Props.value settings.characters]
        ]
        
    ]
]




suggestionWidget :: Settings -> Widget HTML Password
suggestionWidget settings = do
    -- bytes <- liftAff $ randomBytes settingsRecord.length
    -- button [Props.onClick]    [text "Suggestion"]    $> (suggestPassword settings bytes)
    button [Props.onClick]  [text "Suggestion"] $> (Password (show settings.length))

widget :: Settings -> Widget HTML Password
-- widget settings@(Settings settingsRecord) =
--     button [Props.onClick] [text ("Password Generator " <> (show settingsRecord.length))] $> (Password "ciao ciao")
widget settings = do
    value :: Either Settings Password <- (map Left (settingsWidget settings)) <|> (map Right (suggestionWidget settings))
    -- value :: Data <- P (settingsWidget settings) <|> P (suggestionWidget settings)
    case value of
        Left settings' -> widget settings'
        Right password -> pure password
