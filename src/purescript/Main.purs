module Main where

import Components.Main as Components.Main
import Control.Bind (bind)
import Data.Unit (Unit)
import Effect (Effect)
import Halogen.Aff.Util (runHalogenAff, awaitBody)
import Halogen.VDom.Driver (runUI)
import Web.HTML.HTMLElement (HTMLElement)

main :: Effect Unit
main = runHalogenAff do
    body::HTMLElement <- awaitBody
    runUI Components.Main.component defaultInput body

defaultInput :: Components.Main.Input
defaultInput = { length: 8 }
