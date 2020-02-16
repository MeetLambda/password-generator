module Components.Validation where

import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Category (identity)
import Control.Monad (class Monad)
import Control.Semigroupoid ((<<<))
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor (map)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int as Int
import Data.Lens (preview)
import Data.Maybe (Maybe, maybe)
import Data.Ord ((>))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Formless (FormFieldResult, _Error)
import Formless.Validation (Validation(..), hoistFnE_)

-- | Unpacks errors to render as a string
showError :: ∀ e o. ToText e => FormFieldResult e o -> Maybe String
showError = map toText <<< preview _Error

class ToText item where
  toText :: item -> String

instance toTextString :: ToText String where
  toText = identity


--------------------
-- Errors
--------------------
data FieldError
    = InvalidInt String
    | TooShort Int
    | TooLong Int

derive instance genericFieldError :: Generic FieldError _
instance showFieldError :: Show FieldError where
  show = genericShow

instance toTextFieldError :: ToText FieldError where
    toText (InvalidInt str) = "Could not parse \"" <> str <> "\" to a valid integer."
    toText (TooShort n) = "You must enter at least " <> show n <> " characters."
    toText (TooLong n) = "You must enter less than " <> show n <> " characters."
    -- toText InvalidEmail = "That email is not valid."
    -- toText EmailInUse = "That email is already being used."
    -- toText (NotEqual str0 str1) = "This field contains \"" <> str1 <> "\" but must be equal to \"" <> str0 <> "\" to validate."
    -- toText (NotEnoughMoney) = "You don't have that much money."



--------------------
-- Formless Validation
--------------------
strIsInt :: ∀ form m. Monad m => Validation form m FieldError String Int
strIsInt = hoistFnE_ $ \str -> maybe (Left $ InvalidInt str) Right (Int.fromString str)

--------------------
-- Formless Async Validation
--------------------
enoughMoney :: ∀ form m. MonadAff m => Validation form m FieldError Int Int
enoughMoney = Validation \_ i -> do
  -- Let's check if we have enough money...
  _ <- liftAff $ delay $ Milliseconds 5000.0
  pure $ if (i > 1000)
    then Left (TooLong i)
    else pure i
