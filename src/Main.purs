module Main where

import Prelude

import Effect (Effect)
import Flame as Flame
import View as View
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main = do
  Flame.mount_ (QuerySelector "body")
    { init : View.init
    , view : View.view
    , update : View.update
    , subscribe : View.subscribe
    }
