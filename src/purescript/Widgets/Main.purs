module Widgets.Main where

import Concur.Core (Widget)
import Concur.Core.FRP (dyn, hold, loopS, step, stateLoopS, loopW, display, justEffect)
import Concur.React (HTML)
import Concur.React.DOM (text, h4)
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Trans.Class (lift)
import Control.Semigroupoid ((<<<))
import Data.Function (($))
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Set (singleton)
import Data.Show (show)
import Effect (Effect)
import Effect.Aff (Aff, forkAff, joinFiber)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Widgets.PasswordGenerator (AsyncValue(..), AsyncValue, Password, Settings, settingsWidget, randomPassword)

defaultSettings = {
    length              : 24,
    characters          : "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
} :: Settings

-- widget :: forall a. Widget HTML a
-- widget = do
--     -- password :: Widgets.PasswordGenerator.Password <- Widgets.PasswordGenerator.widget defaultSettings (Widgets.PasswordGenerator.Loading Nothing (Widgets.PasswordGenerator.randomPassword defaultSettings.length defaultSettings.characters))
--     let passwordComputation  :: Widgets.PasswordGenerator.Settings -> Aff Widgets.PasswordGenerator.Password
--         passwordComputation settings = (Widgets.PasswordGenerator.randomPassword settings.length settings.characters)
--     -- let defaultPasswordValue = Widgets.PasswordGenerator.Loading Nothing
--     let defaultPasswordValue = Widgets.PasswordGenerator.Loading (Just (Widgets.PasswordGenerator.Password "- - -"))
--     -- password :: Widgets.PasswordGenerator.Password <- Widgets.PasswordGenerator.widget defaultSettings (Widgets.PasswordGenerator.AsyncValueWithComputation passwordComputation defaultPasswordValue)
--     password :: Widgets.PasswordGenerator.Password <- Widgets.PasswordGenerator.widget defaultSettings (Widgets.PasswordGenerator.AsyncValueWithComputation passwordComputation defaultPasswordValue)
--     liftEffect (log ("PASSWORD: " <> (show password)))
--     widget
            

{-

--  A Widget can be considered to be a one-shot Event. 
--  Signals then are never-ending widget loops that allow access to their last return value.
--  This last produced value allows composition with other widgets even for never-ending widgets.

dyn        :: forall b a m. Monad m   =>      SignalT m a        -> m b                             --  Consume a closed signal to make a widget
oneShot    :: forall   a m. Monad m   =>      SignalT m (Maybe a) -> m a                            --  Run a signal once and return its value

demand     :: forall   a m. Monad m   =>             SignalT m (Maybe a)  -> m a                                        --  Very useful to embed a signal in the middle of a widget
demand'    :: forall   a m. Monad m   => (Maybe a -> SignalT m (Maybe a)) -> m a
demandLoop :: forall s a m. Monad m   => Alternative m => s -> (s -> SignalT m (Either s a)) -> m a                     --  A Common pattern is demand + stateLoopS
stateLoopS :: forall s a m. Monad m   => Alternative m => s -> (s -> SignalT m (Either s a)) -> SignalT m (Maybe a)     --  A generalisation of `loopS` where, you have an inner loop state `s` and a final result `a`. The loop continues as long as `Left s` is returned. And ends when `Right a` is returned.
debounce   :: forall   a m. Monad m   => Alt m => MonadAff m => Number -> a -> (a -> m a) -> SignalT m a                --  Debounced output from a widget wrapped into a signal

display    :: forall     m.                   m (SignalT m Unit) -> SignalT m Unit                  --  Display a widget which returns a continuation
step       :: forall   a m.              a -> m (SignalT m a)    -> SignalT m a                     --  Construct a signal from an initial value, and a step widget
loopS      :: forall   a m. Monad m   => a -> (a -> SignalT m a) -> SignalT m a                     --  Loop a signal so that the return value is passed to the beginning again.
loopW      :: forall   a m. Monad m   => a -> (a -> m a)         -> SignalT m a                     --  Create a signal which repeatedly invokes a widget function for values, looping in the prev value.

hold       :: forall   a m. Monad m   => a -> m a -> SignalT m a
foldp      :: forall b a m. Functor m => (a -> b -> a) -> a -> SignalT m b -> SignalT m a           --  Loop a signal so that the return value is passed to the beginning again. This can be used to implement simple stateful Signals. e.g. `counter = fold (\n _ -> n+1) 0 clicks`

always     :: forall   a m. Monad m   => Alternative m => a -> SignalT m a                          --  A constant signal
update     :: forall   a m.                                    SignalT m a -> m (SignalT m a)       --  Update signal to a new value

poll       :: forall   a m. Monad m   =>                       SignalT m (m a) -> m (SignalT m a)   --  Construct a signal by polling a signal with a nested widget for values
hold       :: forall   a m. Monad m   =>                  a -> m a -> SignalT m a                   --  Create a signal which repeatedly invokes a widget for values (eg. `signal False checkbox` will return a signal which reflects the current value of the checkbox).

fireOnce   :: forall   a m. Monad m   => Plus m => m a -> SignalT m (Maybe a)                       --  Fires a widget once then stop. This will reflow when a parent signal reflows Starts as Nothing. Then switches to Just returnVal after the Widget is done
fireOnce_  :: forall   a m. Monad m   => Plus m => m Unit -> SignalT m Unit                         --  Similar to fireOnce, but discards the return value

justWait   :: forall b a m.                  Monad m => Alternative m => b -> SignalT m (Maybe a) -> (a -> SignalT m b) -> SignalT m b      --  Wait until we get a Just value from a signal
justEffect :: forall b a m. MonadEffect m => Monad m => Alternative m => b -> Effect a -> (a -> SignalT m b) -> SignalT m b                 --  Run an effectful computation, and do something with the result

-}

defaultPassword :: AsyncValue Password
defaultPassword = Loading Nothing

passwordGenerator :: Settings -> Aff Password
passwordGenerator settings = randomPassword settings.length settings.characters

widget :: forall a. Widget HTML a
widget = run defaultSettings defaultPassword passwordGenerator
    where
        run :: forall a. Settings -> AsyncValue Password -> (Settings -> Aff Password) -> Widget HTML a
        run settings passwordAsyncValue passwordGenerator = do
            settings' :: Settings <- dyn $ do
                -- s :: Settings <- hold  settings (settingsWidget settings)
                s :: Settings <- loopW  settings (\s -> settingsWidget s)
                display (text $ "Settings: " <> show s)
                --  justEffect :: forall b a m. MonadEffect m => Monad m => Alternative m => b -> Effect a -> (a -> SignalT m b) -> SignalT m b                 --  Run an effectful computation, and do something with the result
                -- p <- justEffect passwordAsyncValue (liftAff $ joinFiber $ liftAff $ forkAff $ passwordGenerator s) 
                pure s
            liftEffect (log $ "PASSOWORD: " <> show settings')
            run settings' passwordAsyncValue passwordGenerator

        -- passwordEffect :: Settings -> (Settings -> Aff Password) -> Effect (AsyncValue Password)
        -- passwordEffect s pg = do
        --     p' <- do
        --         fiber <- liftAff $ forkAff $ pg s
        --         p <- liftAff $ joinFiber fiber
        --         pure $ Done p
        --     pure p'


