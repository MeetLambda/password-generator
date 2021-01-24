module Main where

import Control.Bind (bind)
import Data.Unit (Unit)
import Effect (Effect)
import Concur.React.Run (runWidgetInDom)
import Widgets.Main as Widgets.Main


main :: Effect Unit
main = do
    runWidgetInDom "app" Widgets.Main.widget
