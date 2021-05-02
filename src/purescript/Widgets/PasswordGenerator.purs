module Widgets.PasswordGenerator where
  
import Bytes (Bytes, foldMapBytesToString)
import Concur.Core (Widget)
import Concur.Core.Gen (runWidget)
import Concur.Core.FRP (Signal, dyn, step, display)
import Concur.React (HTML)
import Concur.React.DOM (div, div', text, h1, h2, h4, ul, li, a, p, span, button, form, label, input, fieldset, legend)
import Concur.React.Props (placeholder, value)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Apply ((<*>))
import Control.Bind (bind, discard, (=<<), (>>=))
import Control.Monad (class Monad)
import Control.Monad.Rec.Class (forever, untilJust)
import Control.Monad.State.Trans (StateT, runStateT)
import Control.Parallel (parSequence)
import Control.Plus (empty)
import Control.Semigroupoid ((<<<), (>>>))
import Data.Boolean (otherwise)
import Data.DateTime.Instant (unInstant, toDateTime)
import Data.Either (Either(..), note, hush)
import Data.Formatter.DateTime (Formatter, format, parseFormatString)
import Data.Function (identity, ($), flip)
import Data.Functor (map, void, (<$), ($>), (<$>))
import Data.Int (fromString)
import Data.Lens.Lens (Lens'(..), lens)
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe, maybe)
import Data.Monoid (mempty)
import Data.Newtype (class Newtype)
import Data.Ord ((<=), (<), (>), (>=))
import Data.Ring ((-))
import Data.Semigroup (append, (<>))
import Data.Semiring ((+))
import Data.Set (member)
import Data.Show (show, class Show)
import Data.String.CodePoints (length, take, drop)
import Data.String.Common (null)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..), negateDuration)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, delay, forkAff, joinFiber, launchAff, try)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console as Effect.Console
import Effect.Fortuna (randomBytes)
import Effect.Now (now)
import Formless as Formless
import React.DOM.Dynamic (a, p, s)
import React.SyntheticEvent (SyntheticEvent_)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Document (characterSet)
import Web.HTML.Event.EventTypes (offline)
import Web.HTML.HTMLHyperlinkElementUtils (password)

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
                repeatStringUpToSize' n "" a = ""
                repeatStringUpToSize' n s  a | (length a) + (length s) <= n = repeatStringUpToSize' n s (a <> s)
                repeatStringUpToSize' n s  a = a

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

suggestionWidget :: AsyncValue Password -> Widget HTML PasswordEvent
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
        button [Props.onClick]  [text "New suggestion"] $> RegeneratePassword,
        button [Props.onClick]  [text "return"] $> (ReturnPassword password)
    ]

data PasswordEvent = RegeneratePassword | ReturnPassword Password
data Event = UpdateSettings Settings | UpdatePassword PasswordEvent


suggestionWidget' :: AsyncValue Password -> Widget HTML (AsyncValue Password)
suggestionWidget' (Loading placeholder) = do
    liftEffect (Effect.Console.log $ "suggestionWidget - LOADING: " <> show placeholder)
    div [Props.className "password loading"] [
        input [Props._type "text", Props.disabled true, Props.defaultValue placeholderValue],
        button [Props.disabled true]  [text "New suggestion"],
        button [Props.disabled true]  [text "return"]
    ]
    where
        placeholderValue :: String
        placeholderValue = fromMaybe "---" (map passwordValue placeholder)
suggestionWidget' (Done password) = do
    liftEffect (Effect.Console.log $ "suggestionWidget - DONE: " <> passwordValue password)
    div [Props.className "password"] [
        input [Props._type "text", Props.value $ passwordValue password],
        button [Props.onClick]  [text "New suggestion"] $> Loading (Just password),
        button [Props.onClick]  [text "return"] $> (Done password)
    ]


-- ============================================================================

data AsyncValueWithComputation a = AsyncValueWithComputation (Aff a) (AsyncValue a)
data AsyncValue a = Loading (Maybe a) | Done a

{-
asyncWidget :: Settings -> AsyncValueWithComputation Password -> Widget HTML Password
asyncWidget settings (AsyncValueWithComputation computation value) = div [Props.className "asyncWidget"] [ asyncWidgetContent ]
    where
        asyncWidgetContent :: Widget HTML Password
        asyncWidgetContent = do
            password <- case value of
                Loading placeholder -> do
                    liftEffect (Effect.Console.log "loading")
                    fiber :: Fiber Password <- liftAff $ forkAff computation
                    password <- renderLoading placeholder fiber
                    pure password
                Done password -> do
                    liftEffect (Effect.Console.log "done")
                    pure password
            liftEffect (Effect.Console.log $ "password: " <> passwordValue password)
            render settings (Done password)

        renderLoading :: (Maybe Password) -> (Fiber Password) -> Widget HTML Password
        renderLoading placeholder fiber = (render settings (Loading placeholder)) <|> (liftAff $ joinFiber fiber)

        render :: Settings -> AsyncValue Password -> Widget HTML Password
        render s v = do
            event :: Event  <-  (map UpdateSettings (settingsWidget s))
                            <|> (map UpdatePassword (suggestionWidget v))
            case event of
                UpdateSettings settings' -> asyncWidget settings' (AsyncValueWithComputation computation v)
                UpdatePassword passwordEvent -> case passwordEvent of
                    RegeneratePassword -> asyncWidget settings (AsyncValueWithComputation computation v)
                    ReturnPassword password' -> pure password'
-}
-- ============================================================================

asyncWidgetRunner :: Settings -> AsyncValueWithComputation Password -> Widget HTML Password
asyncWidgetRunner settings (AsyncValueWithComputation computation value) = div [Props.className "asyncWidgetRunner"] [ (untilJust asyncWidgetContent) ]
    where
        asyncWidgetContent :: Widget HTML (Maybe Password)
        asyncWidgetContent = do
            password :: (Maybe Password) <- case value of
                Loading placeholder -> do
                    liftEffect (Effect.Console.log "loading")
                    fiber :: Fiber Password <- liftAff $ forkAff computation
                    password <- renderLoading placeholder fiber
                    pure password
                Done password -> do
                    liftEffect (Effect.Console.log "done")
                    pure $ Just password
            liftEffect (Effect.Console.log $ "password: " <> show (map passwordValue password))
            case password of
                Nothing -> pure Nothing
                Just password ->  render settings (Done password)

        renderLoading :: (Maybe Password) -> (Fiber Password) -> Widget HTML (Maybe Password)
        renderLoading placeholder fiber = (render settings (Loading placeholder)) <|> computeFiber fiber

        computeFiber fiber = do
            password <- (liftAff $ joinFiber fiber)
            pure $ Just password

        render :: Settings -> AsyncValue Password -> Widget HTML (Maybe Password)
        render s v = do
            event :: Event  <-  (map UpdateSettings (settingsWidget s))
                            <|> (map UpdatePassword (suggestionWidget v))
            case event of
                UpdateSettings settings' -> pure Nothing -- asyncWidget settings' (AsyncValueWithComputation computation v)
                UpdatePassword passwordEvent -> case passwordEvent of
                    RegeneratePassword -> pure Nothing -- asyncWidget settings (AsyncValueWithComputation computation v)
                    ReturnPassword password' -> pure (Just password')
        
-- ============================================================================

outerComponent :: Settings -> Widget HTML Password
outerComponent defaultSettings = div [Props.className "outerComponent"] [ innerComponent defaultSettings ]
    where
        innerComponent :: Settings -> Widget HTML Password
        innerComponent settings = do
            s <- div [Props.className "innerComponent"] [
                settingsWidget settings
                -- untilJust $ settingsComponent settings
            ]
            pure $ Password "pippo"

        -- settingsComponent :: Settings -> Widget HTML (Maybe Password)
        -- settingsComponent settings -> 

-- ====================================
--
--  Async execution
--
--  - https://blog.drewolson.org/asynchronous-purescript
--  - https://github.com/JordanMartinez/purescript-jordans-reference/tree/latestRelease/21-Hello-World/02-Effect-and-Aff

-- ============================================================================

widget :: Settings -> AsyncValueWithComputation Password -> Widget HTML Password
widget s v = div [] [
    -- asyncWidget s v,
    -- outerComponent s,
    -- div [] [
    --     h1 [] [text "dyn"],
    --     dyn $ helloSignal ""
    -- ],
    -- div [] [
    --     h1 [] [text "step"],
    --     dyn $ helloSignal ""
    -- ],
    asyncWidgetRunner s v,
    -- signalComponent s,
    clockWidget
]

-- ============================================================================

-- clockWidget :: forall a. Widget HTML a
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
{-
helloWidget :: forall a. Widget HTML a
helloWidget = do
    p <- button [Props.onClick] [text "Say hello"]
    void $ button [Props.onClick] [text "Say hello again"]  
    text "Hello!"


helloWidget' :: forall a. Widget HTML a
helloWidget' = do
    greetings :: String <- div [] [
        "Hello" <$ button [Props.onClick] [text "Say Hello"],
        "Namaste" <$ button [Props.onClick] [text "Say Namaste"]
    ]
    div [] [text (greetings <> " Sailor!")]

helloWidget'' :: forall a. Widget HTML a
helloWidget'' = div [] [ message ]
    where
        message = do
            greetings :: String <- "Hello" <$ button [Props.onClick] [text "Say Hello"] <|> "Namaste" <$ button [Props.onClick] [text "Say Namaste"]
            text (greetings <> " Sailor!")
-}

helloWidget :: forall a. Widget HTML a
helloWidget = do
    greetings :: String <- div [] [
        "Hello" <$ button [Props.onClick] [text "Say Hello"],
        "Namaste" <$ button [Props.onClick] [text "Say Namaste"]
    ]
    div [] [text (greetings <> " Sailor!")]

{-
--
--  S I G N A L
--
hello :: forall a. Widget HTML a
hello = do
    greeting <- div' [
        "Hello" <$ button [Props.onClick] [text "Say Hello"],
        "Namaste" <$ button [Props.onClick] [text "Say Namaste"]
    ]
    a <- text (greeting <> " Sailor") <|> button [Props.onClick] [text "restart"]
    hello
--}

helloSignal :: String -> Signal HTML String
helloSignal s = step s do
    greeting <- div' [
        "Hello" <$ button [Props.onClick] [text "Say Hello"],
        "Namaste" <$ button [Props.onClick] [text "Say Namaste"]
    ]
    a <- text (greeting <> " Sailor") <|> button [Props.onClick] [text "restart"]
    pure (helloSignal greeting)

-- ============================================================================
{-

dyn      :: forall b a m. Monad m => SignalT m a        -> m b

display  :: forall     m.                   m (SignalT m Unit) -> SignalT m Unit
step     :: forall   a m.              a -> m (SignalT m a)    -> SignalT m a
loopS    :: forall   a m. Monad m   => a -> (a -> SignalT m a) -> SignalT m a
loopW    :: forall   a m. Monad m   => a -> (a -> m a)         -> SignalT m a

hold     :: forall   a m. Monad m   => a -> m a -> SignalT m a
fireOnce :: forall   a m. Monad m   => Plus m => m a -> SignalT m (Maybe a)
foldp    :: forall b a m. Functor m => (a -> b -> a) -> a -> SignalT m b -> SignalT m a

-}


-- ============================================================================
