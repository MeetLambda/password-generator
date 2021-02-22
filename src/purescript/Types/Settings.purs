module Types.Settings where

import Control.Applicative (pure)
import Control.Semigroupoid ((<<<))
import Data.Foldable (foldr)
import Data.Function (($))
import Data.Options (options)
import Data.Set (Set, fromFoldable, toUnfoldable, union, empty)
import Data.Show (class Show)
import Data.String.CodeUnits (toCharArray, fromCharArray)
import Effect (Effect)
import Payload.Internal.Utils (toLowerCase)
import Protobuf.Common (Bytes)

data    Password = Password String
-- derive instance showPassword :: Show Password
instance showPassword :: Show Password where
  show (Password password) = password

-- type  CharSet = Set Char

stringToSet :: String -> (Set Char)
stringToSet = fromFoldable <<< toCharArray

setToChars :: (Set Char) -> String
setToChars s = fromCharArray $ toUnfoldable s

characterSetsToString :: (Set (Set Char)) -> String
characterSetsToString xs = fromCharArray $ toUnfoldable (foldr union empty xs)

uppercaseLetters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" :: String

uppercaseLettersSet = stringToSet uppercaseLetters                  :: Set Char
lowercaseLettersSet = stringToSet $ toLowerCase uppercaseLetters    :: Set Char
numbersSet          = stringToSet "0123456789"                      :: Set Char
spacesSet           = stringToSet " "                               :: Set Char
weirdcharsSet       = stringToSet "!#$%…"                           :: Set Char

-- type Settings = {
--     length :: Int,
--     characterSets :: Set CharSet,
--     characters :: String
-- }
-- derive instance newtypeSettings :: Newtype Settings _

type Options = {
    uppercaseLetters    :: Boolean,
    lowercaseLetters    :: Boolean,
    numbers             :: Boolean,
    spaces              :: Boolean,
    weirdchars          :: Boolean
}

charactersWithOptions :: Options -> (Set Char)
charactersWithOptions options = foldr union empty [
    (if options.uppercaseLetters    then uppercaseLettersSet    else empty),
    (if options.lowercaseLetters    then lowercaseLettersSet    else empty),
    (if options.numbers             then numbersSet             else empty),
    (if options.spaces              then spacesSet              else empty),
    (if options.weirdchars          then weirdcharsSet          else empty)
]

type Settings = {
    length      :: Int,
    options     ::  Options,
    characters  :: String
}


-- =========================================================

suggestPassword :: Settings -> (Int -> Effect Bytes) -> Password
suggestPassword settings prng = Password "pippo"
