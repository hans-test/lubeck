
{-
A demo application using Lubeck libraries.
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
  , StrictData
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

import BasePrelude hiding (rotate)
import Lubeck.FRP
import Lubeck.Drawing
import Lubeck.Str
import qualified Data.Colour.Names as Colors
import qualified Data.Colour.Names as C

import Lubeck.App -- KeyEvent
import Lubeck.Html
import Lubeck.Util
import qualified Web.VirtualDom as VD
import qualified Web.VirtualDom.Html as VD
import qualified Web.VirtualDom.Html.Attributes as VD
import qualified GHCJS.Foreign.Callback as CB
import GHCJS.Types(JSVal, JSString)
import Data.Colour(Colour)

import DemoApp

-- NOTE bleeding edge stuff, only provided by internal/unstable API
import Lubeck.Drawing.Internal.Backend.FastRenderer (runRenderingLoopOn, CanvasElement(..)
  , renderFastDrawing, adaptCoordinates, Renderer, MouseEventType(..), MouseEvent(..)
  , offsetX, offsetY, Context, clearRect
  , prerender, usePrerendered
  )


type Image = Draft Fast

-- | Wrapper layer that detects esc, shows "restarting" for 3 seconds and
-- then reboots the app (by calling topLevel again).
topLevelWithRestart :: Image -> Events KeyEvent -> FRP (Behavior Image)
topLevelWithRestart p kb = do
  restartE :: Events () <- pure $ filterJust $ keyBoardNav <$> kb
  restartDoneE :: Events () <- secondsLater restartE
  be :: Events (Behavior Image) <- reactimateIO $ fmap (const $ topLevel p kb) restartDoneE
  bb :: Behavior (Behavior Image) <- stepper (pure initScreen) (be <> fmap (const $ pure restartScreen) restartE)
  pure $ join bb
  where
    initScreen :: Image
    initScreen = textWithOptions stdTextLarger "Press esc to start!"

    restartScreen :: Image
    restartScreen = textWithOptions stdTextLarger "Restarting..."

    keyBoardNav :: KeyEvent -> Maybe ()
    keyBoardNav (Key 27) = Just ()
    keyBoardNav _        = Nothing

    secondsLater :: Events () -> FRP (Events ())
    secondsLater = reactimateIOAsync . fmap (const $ threadDelay 1000000)

data TopLevelSubNav = ListComp | TreeComp | SideBySide
  deriving (Eq, Show)

-- | Top-level component.
topLevel :: Image -> Events KeyEvent -> FRP (Behavior Image)
topLevel p kb = do
  (setActive, activeB :: Behavior TopLevelSubNav) <- newBehavior ListComp
  listV <- navigateListComp $ whenB (== ListComp) activeB kb
  treeV <- navigateTreeComp p $ whenB (== TreeComp) activeB kb
  subscribeEvent (filterJust $ keyBoardNav <$> kb) setActive
  pure $ mconcat
    [ currentSubNavText <$> activeB
    , ( \v a b -> case v of
        { ListComp   -> a
        ; TreeComp   -> b
        ; SideBySide -> a <> translateX 100 b
        }
      ) <$> activeB
        <*> listV
        <*> treeV
    ]
    where
      keyBoardNav :: KeyEvent -> Maybe TopLevelSubNav
      keyBoardNav (Key 49) = Just ListComp
      keyBoardNav (Key 50) = Just TreeComp
      keyBoardNav (Key 51) = Just SideBySide
      keyBoardNav _        = Nothing

      currentSubNavText :: TopLevelSubNav -> Image
      currentSubNavText = translate (V2 (-390) 170) . textWithOptions stdTextLarger . toStr

      whenB :: (a -> Bool) -> Behavior a -> Events b -> Events b
      whenB p b e = filterJust $ snapshotWith (\pv x -> if p pv then Just x else Nothing) b e

-- | List view.
navigateListComp :: Events KeyEvent -> FRP (Behavior Image)
navigateListComp _ = do
  pure $ pure $ mconcat
    [ mempty
    , fillColor Colors.green $ scale 200 circle
    ]

-- | Tree view.
navigateTreeComp :: Image -> Events KeyEvent -> FRP (Behavior Image)
navigateTreeComp p e = do
  ks :: Behavior KeyEvent <- stepper (Key 0) e
  pure $ flip fmap ks $ \k -> mconcat
    [ mempty
    , translate (V2 40 40) $ p
    , translate (V2 100 40) $ textWithOptions stdTextSmaller (toStr k)
    ]
  where


bang :: Colour Double -> Events () -> FRP (Behavior Image)
bang c e = pure $ pure $ fillColor c (scale 50 circle) <> fillColor C.grey (scale 50 square)
  where
    -- TODO bang
    -- onOff = accumB _


-- componentSignal :: a -> WidgetT r a a -> Events a -> IO (Signal r, Signal a)
-- selectedModuleW :: [Str] -> Image
-- selectedModuleW = mconcat . zipWith (\n d -> translateY n d) [0,10..] . fmap (textWithOptions stdTextSmaller)

stdTextFamily       = First (Just "Gill Sans, sans-serif")
stdText             = mempty { fontFamily = stdTextFamily, fontSize = First (Just "16px"), fontWeight = FontWeightN 500 }
stdTextLarger       = stdText { fontSize = First (Just "19px")}
stdTextEvenLarger   = stdText { fontSize = First (Just "24px")}
stdTextSmaller      = stdText { fontSize = First (Just "14px")}






initApp :: Events KeyEvent -> CanvasElement -> Context -> Renderer -> IO (Context, Behavior Image)
initApp keyE _ c r = do
  -- TODO Prerender something and pass to topLevelWithRestart...
  let expensive = getDraft $ mconcat $ take 150 $ iterate (rotate (turn/150)) $ fillColor C.red $ translateX 400 $ scale 10 circle
  r1 <- prerender expensive r
  appRes <- topLevelWithRestart (Draft $ usePrerendered r1) keyE
  pure (c, appRes)

update :: (Context, Behavior Image) -> Renderer -> MouseEventType -> MouseEvent -> IO ()
update _ c et e = do
  -- writeIORef s (round $ offsetX e, round $ offsetY e)
  pure ()

render :: (Context, Behavior Image) -> Renderer -> IO ()
render (c,i) r = do
  i' <- pollBehavior i
  clearRect c 0 0 800 800
  renderFastDrawing r $ adaptCoordinates (RenderingOptions (P (V2 800 400)) Center False) $ getDraft $
    mconcat
      [ mempty
      , i'
      -- , translate (V2 0 100) $ strokeColor Colors.blue $ textWithOptions stdText $ ("" <> toStr s')
      ]
  pure ()

main :: IO ()
main = do
  print (123 :: Foo)

  (keyU, keyE :: Events KeyEvent) <- newEvent
  subscribeEvent keyE print

  cb <- CB.asyncCallback1 $ \canvasDomNode -> do
        print "Done setup"
        setCanvasSizeRetina (DOMCanvasElement canvasDomNode)
        runRenderingLoopOn (DOMCanvasElement canvasDomNode) (initApp keyE) update render
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
