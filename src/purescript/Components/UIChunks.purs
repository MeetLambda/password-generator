module Components.UIChunks where

import Control.Semigroupoid ((<<<))
import Data.Either (Either(..), either)
import Data.Function (($), const)
import Data.Maybe (maybe)
import Data.Semigroup ((<>))
import DOM.HTML.Indexed (HTMLbutton, HTMLinput)
import DOM.HTML.Indexed.InputType (InputType(..))
import Halogen.HTML as HTML
import Halogen.HTML.Properties as HTML.Properties
import Formless (FormFieldResult(..))
import Components.Validation as Validation

type Plain i p = Array (HTML.HTML i p) -> HTML.HTML i p
class_ :: forall r t. String -> HTML.IProp ( "class" :: String | r ) t
class_ = HTML.Properties.class_ <<< HTML.ClassName

formContent_ :: forall i p. Plain i p
formContent_ content =
  HTML.div
    [ class_ "content" ]
    [ HTML.div
        [ class_ "column has-background-white-bis" ]
        content
    ]

type FieldConfig' = {
    label :: String,
    help :: Either String String,
    placeholder :: String
}

resultToHelp :: forall t e. Validation.ToText e => String -> FormFieldResult e t -> Either String String
resultToHelp str = case _ of
    NotValidated -> Right str
    Validating -> Right "validating..."
    other -> maybe (Right str) Left $ Validation.showError other

field :: forall i p. { label :: String, help :: Either String String } -> Plain i p
field config contents = HTML.div [ class_ "field" ] [
    HTML.div [ class_ "label" ] [
        HTML.text config.label
    ],
    HTML.div [ class_ "control" ] contents,
    case config.help of
        Left str -> helpError_ str
        Right str -> help_ str
]
    where
        help_ str = HTML.p [ class_ "help" ] [ HTML.text str ]
        helpError_ str = HTML.p [ class_ "help is-danger" ] [ HTML.text str ]

input :: forall i p. FieldConfig' -> Array (HTML.IProp HTMLinput p) -> HTML.HTML i p
input config props = field { label: config.label, help: config.help } [
    HTML.input $ [
        HTML.Properties.type_ InputText,
        either (const $ class_ "input is-danger") (const $ class_ "input") config.help,
        HTML.Properties.placeholder config.placeholder
    ] <> props
]

buttonPrimary :: forall i p. Array (HTML.IProp HTMLbutton p) -> Plain i p
buttonPrimary props = HTML.button ([ class_ "button is-link" ] <> props)
