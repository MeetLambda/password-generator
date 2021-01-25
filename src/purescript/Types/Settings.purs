module Types.Settings where

import Control.Applicative (pure)
import Control.Semigroupoid ((<<<))
import Data.Foldable (foldr)
import Data.Function (($))
import Data.Set (Set, fromFoldable, toUnfoldable, union, empty)
import Data.Show (class Show)
import Data.String.CodeUnits (toCharArray, fromCharArray)
import Payload.Internal.Utils (toLowerCase)

import Effect (Effect)
import Protobuf.Common (Bytes)

data    Password = Password String
-- derive instance showPassword :: Show Password
instance showPassword :: Show Password where
  show (Password password) = password

type    CharSet = Set Char

stringToSet :: String -> CharSet
stringToSet = fromFoldable <<< toCharArray

setToChars :: CharSet -> String
setToChars s = fromCharArray $ toUnfoldable s

characterSetsToString :: (Set CharSet) -> String
characterSetsToString xs = fromCharArray $ toUnfoldable (foldr union empty xs)

uppercaseLetters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" :: String

uppercaseLettersSet = stringToSet uppercaseLetters                  :: CharSet
lowercaseLettersSet = stringToSet $ toLowerCase uppercaseLetters    :: CharSet
numbersSet          = stringToSet "0123456789"                      :: CharSet
spacesSet           = stringToSet " "                               :: CharSet
weirdcharsSet       = stringToSet "!#$%"                            :: CharSet

data Settings = Settings {
    length :: Int,
    characterSets :: Set CharSet,
    characters :: String
}


-- =========================================================

suggestPassword :: Settings -> Bytes -> Password
suggestPassword settings bytes = Password "pippo"
