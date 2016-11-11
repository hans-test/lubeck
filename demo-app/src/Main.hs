
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
import Lubeck.Str
import qualified Data.Colour.Names as Colors

#ifdef __GHCJS__
-- =============================================================================
-- Client
-- =============================================================================
import Lubeck.App
import Lubeck.Html
import qualified Web.VirtualDom as VD
import qualified Web.VirtualDom.Html as VD
import qualified Web.VirtualDom.Html.Attributes as VD
import GHCJS.Foreign.Callback as CB
-- import GHCJS.Marshal(toJSVal) -- only need a function to convert Aeson(Value) to JsVal
import GHCJS.Types(JSVal, JSString)
import Lubeck.Drawing.Internal.Backend.FastRenderer (runRenderingLoopOn, CanvasElement(..)
  , renderFastDrawing, adaptCoordinates, Renderer, MouseEventType(..), MouseEvent(..)
  , offsetX, offsetY, Context
  )
#else
-- =============================================================================
-- Server
-- =============================================================================
#endif





type Foo = Int

-- data DagZip a = AtNode a



#ifdef __GHCJS__
-- =============================================================================
-- Client
-- =============================================================================

type Image = Draft Fast

data TopLevelSubNav = ListComp | TreeComp

topLevel :: Events KeyEvent -> FRP (Behavior Image)
topLevel kb = do
  (setActive, activeB :: Signal TopLevelSubNav) <- newBehavior ListComp
  listV <- navigateListComp $ whenB (== ListComp) activeB kb
  treeV <- navigateTreeComp $ whenB (== TreeComp) activeB kb
  pure $ mconcat [renderTopLevelSubNav <$> activeB, listV, treeV]
    where
      renderTopLevelSubNav :: TopLevelSubNav -> Image
      renderTopLevelSubNav = translateY 380 . textWithOptions stdTextLarger . toStr

      whenB :: (a -> Bool) -> Behavior a -> Events b -> Events b
      whenB p b e = filterJust $ snapshotWith (\pv x -> if p pv then Just x else Nothing) b e

navigateListComp :: Events KeyEvent -> FRP (Behavior Image)
navigateListComp _ = do
  pure $ pure $ mconcat
    [ fillColor Colors.green $ scale 200 circle
    ]

navigateTreeComp :: Events KeyEvent -> FRP (Behavior Image)
navigateTreeComp _ = do
  pure $ pure $ mconcat
    [ translate (V2 40 40) $ fillColor Colors.grey $ scale 200 circle
    ]







-- componentSignal :: a -> WidgetT r a a -> Events a -> IO (Signal r, Signal a)

-- selectedModuleW :: [Str] -> Image
-- selectedModuleW = mconcat . zipWith (\n d -> translateY n d) [0,10..] . fmap (textWithOptions stdTextSmaller)



stdTextFamily       = First (Just "Gill Sans, sans-serif")
stdText             = mempty { fontFamily = stdTextFamily, fontSize = First (Just "16px"), fontWeight = FontWeightN 500 }
stdTextLarger       = stdText { fontSize = First (Just "19px")}
stdTextEvenLarger   = stdText { fontSize = First (Just "24px")}
stdTextSmaller      = stdText { fontSize = First (Just "14px")}

type AppState = IORef (Int, Int)

initApp :: CanvasElement -> Context -> Renderer -> IO AppState
initApp _ _ r = do
  newIORef (0,0)

update :: AppState -> Renderer -> MouseEventType -> MouseEvent -> IO ()
update s _ et e = do
  -- print et
  writeIORef s (round $ offsetX e, round $ offsetY e)
  pure ()

render :: AppState -> Renderer -> IO ()
render s r = do
  s' <- readIORef s
  renderFastDrawing r $ adaptCoordinates (RenderingOptions (P (V2 800 400)) Center False) $ getDraft $
    mconcat
      [ mempty
      -- , translate (V2 0 100) $ strokeColor Colors.blue $ textWithOptions stdText $ ("" <> toStr s')
      ]
  pure ()

main :: IO ()
main = do
  print "Initializing"
  (keyU, keyE :: Events KeyEvent) <- newEvent
  subscribeEvent keyE print
  cb <- asyncCallback1 $ \canvasDomNode -> do
        print "Done setup"
        setCanvasSizeRetina (DOMCanvasElement canvasDomNode)
        runRenderingLoopOn (DOMCanvasElement canvasDomNode) initApp update render
        -- creatingCanvasDoneU (DOMCanvasElement canvasDomNode)
  let mainV :: Signal Html = pure $
          VD.staticNode "div" [VD.id "wrap-canvas"] $
            pure $ VD.staticNodeWithCB cb "canvas" [] []
  runAppReactive' (mainV, Just keyU)

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
