
{-
A demo application using Lubeck libraries.

Cross building GHC/GHCJS can be done in various inelegant ways, here we use CPP.

-}


{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , DeriveFunctor
  , TypeFamilies
  , OverloadedStrings
  , NamedFieldPuns
  , BangPatterns
  , ScopedTypeVariables
  , NoImplicitPrelude
  , NoImplicitPrelude
  , GeneralizedNewtypeDeriving
  , CPP
  #-}

{-# OPTIONS_GHC
  -fwarn-incomplete-patterns
  -fno-warn-name-shadowing
  -fno-warn-unused-binds
  -fno-warn-unused-matches
  -fno-warn-unused-imports
  -fno-warn-type-defaults
  -fno-warn-missing-signatures
  -Werror
  -O3
  #-}

module Main where
{-

loadHaskellProject :: FilePath -> Maybe (DAGZipper () String)




-}

-- Shared
import BasePrelude
import Lubeck.FRP
import Lubeck.Drawing
import qualified Data.Colour.Names as Colors

#ifdef __GHCJS__
-- =============================================================================
-- Client
-- =============================================================================
import Lubeck.App(runAppReactive)
import Lubeck.Html(Html)
import qualified Web.VirtualDom as VD
import qualified Web.VirtualDom.Html as VD
import qualified Web.VirtualDom.Html.Attributes as VD
import GHCJS.Foreign.Callback as CB
-- import GHCJS.Marshal(toJSVal) -- only need a function to convert Aeson(Value) to JsVal
import GHCJS.Types(JSVal, JSString)
import Lubeck.Drawing.Internal.Backend.FastRenderer (runRenderingLoopOn, CanvasElement(..)
  , renderFastDrawing, adaptCoordinates, Renderer)
#else
-- =============================================================================
-- Server
-- =============================================================================
#endif





type Foo = Int





#ifdef __GHCJS__
-- =============================================================================
-- Client
-- =============================================================================

stdTextFamily       = First (Just "Gill Sans, sans-serif")
stdText             = mempty { fontFamily = stdTextFamily, fontSize = First (Just "16px"), fontWeight = FontWeightN 500 }
stdTextLarger       = stdText { fontSize = First (Just "19px")}
stdTextEvenLarger   = stdText { fontSize = First (Just "24px")}
stdTextSmaller      = stdText { fontSize = First (Just "14px")}


initApp _ _ r = do
  pure ()
update _ _ _ _ = do
  pure ()
render :: () -> Renderer -> IO ()
render () r = do
  renderFastDrawing r $ adaptCoordinates (RenderingOptions (P (V2 800 400)) Center False) $ getDraft $
    mconcat
      [ mempty
      , fillColor Colors.green $ scale 200 circle
      , translate (V2 40 40) $ fillColor Colors.grey $ scale 200 circle
      , translate (V2 0 100) $ strokeColor Colors.blue $ textWithOptions stdText "Hello!"
      ]
  pure ()

main = do
  print (123 :: Foo)
  cb <- asyncCallback1 $ \canvasDomNode -> do
        print "Done setup"
        setCanvasSizeRetina (DOMCanvasElement canvasDomNode)
        runRenderingLoopOn (DOMCanvasElement canvasDomNode) initApp update render
        -- creatingCanvasDoneU (DOMCanvasElement canvasDomNode)
  let mainV :: Signal Html = pure $
          VD.staticNode "div" [VD.id "wrap-canvas"] $
            pure $ VD.staticNodeWithCB cb "canvas" [] []
  runAppReactive $ mainV

-- Work around Retina downsampling issue
-- a la https://coderwall.com/p/vmkk6a/how-to-make-the-canvas-not-look-like-crap-on-retina
foreign import javascript
  "canvas = $1; canvas.width = 1600; canvas.height = 1600; canvas.style.width = \"800px\"; canvas.style.height = \"800px\"; canvas.getContext('2d').scale(2,2);"
  setCanvasSizeRetina :: CanvasElement -> IO ()

#else
-- =============================================================================
-- Server
-- =============================================================================



main = print (123 :: Foo)

#endif
