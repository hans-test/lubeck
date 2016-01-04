
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections #-}

import Prelude hiding (div)
import qualified Prelude

import qualified Data.Text
import Data.Text(Text)

import GHCJS.VDOM.Event (click, change, submit, stopPropagation, preventDefault, value)
import GHCJS.Types(JSString, jsval)
import GHCJS.VDOM.Element (p, h1, div, text, form, button, img, hr, custom)
import GHCJS.VDOM.Attribute (src, width, class_)

import FRP2
import App (Html, runApp)

type Widget i o = Sink o -> i -> Html
type Widget' a  = Widget a a

update :: E () -> IO (R (Text, Maybe (IO ())))
update = foldpR step initial
  where
    initial = ("Hello Web!", Nothing)
    step () (model,_) = (model, Nothing)

render :: Widget Text ()
render actions model = h1 () [text $ textToJSString model]

-- MAIN

main :: IO ()
main = runApp update render

textToJSString :: Text -> JSString
textToJSString = fromString . Data.Text.unpack
