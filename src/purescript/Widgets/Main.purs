module Widgets.Main where

import Concur.Core (Widget)
import Concur.Core.FRP (SignalT(..), always, demandLoop, display, dyn, demand, fireOnce, fireOnce_, hold, justEffect, justWait, loopS, loopW, oneShot, stateLoopS, step, update)
import Concur.Core.Props (mkProp)
import Concur.React (HTML)
import Concur.React.DOM (text, h4, div, button)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Alternative (class Alternative)
import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Control.Comonad (extract)
import Control.Monad (class Monad)
import Control.Monad.Trans.Class (lift)
import Control.Plus (class Plus)
import Control.Semigroupoid ((<<<))
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor (map, void, (<$), ($>), (<$>))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Semigroup ((<>))
import Data.Set (singleton)
import Data.Show (show)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, forkAff, joinFiber, launchAff, runAff)
import Effect.Aff.Class (liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Widgets.PasswordGenerator (AsyncValue(..), AsyncValue, Password(..), PasswordEvent(..), Settings, randomPassword, settingsWidget, suggestionWidget, suggestionWidget', suggestionWidget'')

defaultSettings = {
    length              : 24,
    characters          : "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
} :: Settings


{-

--  A Widget can be considered to be a one-shot Event. 
--  Signals then are never-ending widget loops that allow access to their last return value.
--  This last produced value allows composition with other widgets even for never-ending widgets.

dyn        :: forall b a m. Monad m =>      SignalT m a         -> m b                              --  Consume a closed signal to make a widget
oneShot    :: forall   a m. Monad m =>      SignalT m (Maybe a) -> m a                              --  Run a signal once and return its value (aka demand)

demand     :: forall   a m. Monad m =>             SignalT m (Maybe a)  -> m a                      --  Very useful to embed a signal in the middle of a widget (aka oneShot)
demand'    :: forall   a m. Monad m => (Maybe a -> SignalT m (Maybe a)) -> m a  

display    :: forall     m.                 m (SignalT m Unit) -> SignalT m Unit                    --  Display a widget which returns a continuation
step       :: forall   a m.            a -> m (SignalT m a)    -> SignalT m a                       --  Construct a signal from an initial value, and a step widget
loopS      :: forall   a m. Monad m => a -> (a -> SignalT m a) -> SignalT m a                       --  Loop a signal so that the return value is passed to the beginning again.
loopW      :: forall   a m. Monad m => a -> (a -> m a)         -> SignalT m a                       --  Create a signal which repeatedly invokes a widget function for values, looping in the prev value.

always     :: forall   a m. Monad m => Alternative m => a -> SignalT m a                            --  A constant signal
update     :: forall   a m.                                  SignalT m a -> m (SignalT m a)         --  Update signal to a new value

poll       :: forall   a m. Monad m => SignalT m (m a) -> m (SignalT m a)                           --  Construct a signal by polling a signal with a nested widget for values
hold       :: forall   a m. Monad m => a -> m a        -> SignalT m a                               --  Create a signal which repeatedly invokes a widget for values (eg. `signal False checkbox` will return a signal which reflects the current value of the checkbox).

fireOnce   :: forall   a m. Monad m => Plus m => m a    -> SignalT m (Maybe a)                      --  Fires a widget once then stop. This will reflow when a parent signal reflows Starts as Nothing. Then switches to Just returnVal after the Widget is done
fireOnce_  :: forall   a m. Monad m => Plus m => m Unit -> SignalT m Unit                           --  Similar to fireOnce, but discards the return value

justWait   :: forall b a m.                  Monad m => Alternative m => b -> SignalT m (Maybe a) -> (a -> SignalT m b) -> SignalT m b      --  Wait until we get a Just value from a signal
justEffect :: forall b a m. MonadEffect m => Monad m => Alternative m => b -> Effect a            -> (a -> SignalT m b) -> SignalT m b      --  Run an effectful computation, and do something with the result

foldp      :: forall b a m. Functor m => (a -> b -> a) -> a -> SignalT m b -> SignalT m a           --  Loop a signal so that the return value is passed to the beginning again. This can be used to implement simple stateful Signals. e.g. `counter = fold (\n _ -> n+1) 0 clicks`

demandLoop :: forall s a m. Monad m => Alternative m => s -> (s -> SignalT m (Either s a)) -> m a                       --  A Common pattern is demand + stateLoopS
stateLoopS :: forall s a m. Monad m => Alternative m => s -> (s -> SignalT m (Either s a)) -> SignalT m (Maybe a)       --  A generalisation of `loopS` where, you have an inner loop state `s` and a final result `a`. The loop continues as long as `Left s` is returned. And ends when `Right a` is returned.
debounce   :: forall   a m. Monad m => Alt m => MonadAff m => Number -> a -> (a -> m a) -> SignalT m a                  --  Debounced output from a widget wrapped into a signal
-}

widget :: forall a. Widget HTML a
widget = run defaultSettings defaultPassword
    where
        run :: forall a'. Settings -> AsyncValue Password -> Widget HTML a'
        run settings passwordAsyncValue = do -- Widget
            password <- demand $ runComponent (Tuple settings passwordAsyncValue)
            liftEffect (log $ "DONE: " <> show password)
            text "DONE"

        -- runComponent :: (Tuple Settings (AsyncValue Password)) -> SignalT (Widget HTML) (Either (Tuple Settings (AsyncValue Password)) Password)
        runComponent :: (Tuple Settings (AsyncValue Password)) -> SignalT (Widget HTML) (Maybe Password)
        runComponent (Tuple _settings _av) = do -- SignalT
            --  s => (Tuple Settings (AsyncValue Password))
            --  a => Password
            stateLoopS (Tuple _settings _av) (\(Tuple settings av) -> do                
                logSignal $ "[1] signal"
                let ts = map (\s -> Tuple s av) (loopW settings settingsWidget)
                t :: Tuple Settings (AsyncValue Password) <- ts -- map (\s -> Tuple s av) (loopW settings settingsWidget)

                logSignal $ "[2] signal"
                p :: AsyncValue Password <- justWait (snd t) (fireOnce (computePassword (fst t))) always

                -- mp :: Maybe (AsyncValue Password) <- (fireOnce (computePassword (fst t)))
                -- pure mp

                -- p' :: AsyncValue Password <- loopW p suggestionWidget''
                -- map (returnValue t) (loopW RegeneratePassword (\_ -> suggestionWidget p))

                -- fireOnce (suggestionWidget p)
                -- pe :: PasswordEvent <- justWait RegeneratePassword (fireOnce (suggestionWidget p)) always
                -- map (returnValue t) $ justWait RegeneratePassword (fireOnce (\_ -> suggestionWidget p)) always

                logSignal $ "[3] signal: " <> show p

                --  poll       :: forall   a m. Monad m => SignalT m (m a) -> m (SignalT m a)                           --  Construct a signal by polling a signal with a nested widget for values

                -- map (returnValue (fst t)) (loopW p (\_ -> suggestionWidget'' p))
                map (returnValue (fst t)) (justWait p (fireOnce (suggestionWidget'' p)) always)



            )

        returnValue :: Settings -> AsyncValue Password -> (Either (Tuple Settings (AsyncValue Password)) Password) 
        returnValue s av =
            case av of
                Loading l -> Left (Tuple s av)
                Done p -> Left (Tuple s av)
                Return p -> Right p
-- ========================================================================================

logSignal :: forall m. Monad m => Plus m => MonadEffect m => String -> SignalT m Unit
logSignal s = fireOnce_ (liftEffect (log s))

defaultPassword :: AsyncValue Password
defaultPassword = Loading Nothing

computePassword :: Settings -> Widget HTML (AsyncValue Password)
computePassword s = liftAff $ map Done (randomPassword s.length s.characters)
