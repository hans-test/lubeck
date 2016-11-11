
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
import Lubeck.Util
import qualified Web.VirtualDom as VD
import qualified Web.VirtualDom.Html as VD
import qualified Web.VirtualDom.Html.Attributes as VD
import GHCJS.Foreign.Callback as CB
import GHCJS.Types(JSVal, JSString)

-- NOTE bleeding edge stuff, only provided by internal/unstable API
import Lubeck.Drawing.Internal.Backend.FastRenderer (runRenderingLoopOn, CanvasElement(..)
  , renderFastDrawing, adaptCoordinates, Renderer, MouseEventType(..), MouseEvent(..)
  , offsetX, offsetY, Context, clearRect
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

-- | Wrapper layer that detects esc, shows "restarting" for 3 seconds and
-- then reboots the app (by calling topLevel again).
topLevelWithRestart :: Events KeyEvent -> FRP (Behavior Image)
topLevelWithRestart kb = do
  restartE :: Events () <- pure $ filterJust $ keyBoardNav <$> kb
  restartDoneE :: Events () <- secondsLater restartE
  be :: Events (Behavior Image) <- reactimateIO $ fmap (const $ topLevel kb) restartDoneE
  bb :: Behavior (Behavior Image) <- stepper (pure initScreen) (be <> fmap (const $ pure restartScreen) restartE)
  pure $ join bb
  where
    initScreen :: Image
    initScreen = textWithOptions stdTextLarger "Press esc to start"

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
topLevel :: Events KeyEvent -> FRP (Behavior Image)
topLevel kb = do
  (setActive, activeB :: Behavior TopLevelSubNav) <- newBehavior ListComp
  listV <- navigateListComp $ whenB (== ListComp) activeB kb
  treeV <- navigateTreeComp $ whenB (== TreeComp) activeB kb
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
navigateTreeComp :: Events KeyEvent -> FRP (Behavior Image)
navigateTreeComp _ = do
  pure $ pure $ mconcat
    [ mempty
    , translate (V2 40 40) $ fillColor Colors.grey $ scale 200 circle
    ]







-- componentSignal :: a -> WidgetT r a a -> Events a -> IO (Signal r, Signal a)

-- selectedModuleW :: [Str] -> Image
-- selectedModuleW = mconcat . zipWith (\n d -> translateY n d) [0,10..] . fmap (textWithOptions stdTextSmaller)



stdTextFamily       = First (Just "Gill Sans, sans-serif")
stdText             = mempty { fontFamily = stdTextFamily, fontSize = First (Just "16px"), fontWeight = FontWeightN 500 }
stdTextLarger       = stdText { fontSize = First (Just "19px")}
stdTextEvenLarger   = stdText { fontSize = First (Just "24px")}
stdTextSmaller      = stdText { fontSize = First (Just "14px")}

initApp :: CanvasElement -> Context -> Renderer -> IO Context
initApp _ c r = do
  pure c

update :: Context -> Renderer -> MouseEventType -> MouseEvent -> IO ()
update s c et e = do
  -- writeIORef s (round $ offsetX e, round $ offsetY e)
  pure ()

render :: Behavior Image -> Context -> Renderer -> IO ()
render i c r = do
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
  print "Initializing"
  (keyU, keyE :: Events KeyEvent) <- newEvent
  subscribeEvent keyE print
  appRes <- topLevelWithRestart keyE
  cb <- asyncCallback1 $ \canvasDomNode -> do
        print "Done setup"
        setCanvasSizeRetina (DOMCanvasElement canvasDomNode)
        runRenderingLoopOn (DOMCanvasElement canvasDomNode) initApp update (render appRes)
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
