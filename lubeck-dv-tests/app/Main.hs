
{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, ScopedTypeVariables, TypeFamilies
  , FlexibleContexts, BangPatterns, CPP #-}

-- {-# GHC_OPTIONS -ddump-simpl #-}

module Main where

import BasePrelude hiding ((|||), Signal)
import Control.Lens(to, _1, _2, (.~))
import Linear.Affine ((.+^))
import Data.Colour (withOpacity)
import Data.Colour.Names as Colors

-- Debug
import Debug.Trace
import Control.Concurrent(threadDelay)

import Lubeck.DV
import Lubeck.FRP
import Lubeck.Str
import Lubeck.Drawing hiding (text, addProperty)

#ifdef __GHCJS__
import Lubeck.Forms(componentEvent)
import Lubeck.Forms.Button(multiButtonWidget)
import Lubeck.Forms.Select(selectEnumBoundedWidget)
import Lubeck.App(runAppReactive)
import Web.VirtualDom.Html(Html, h1, text)
import qualified Web.VirtualDom as VirtualDom
-- TODO separate/consolidate Html/Svg events
-- Currently they can be used more or less interchangebly

import Web.VirtualDom.Html.Events(Event(..), mousemove)

import           GHCJS.Foreign.Callback(syncCallback,Callback,OnBlocked(..))
#endif


----------

debugHandlers = False

-- TODO handle onmousedown, onmousemove, onmouseout, onmouseup
-- Ref: http://www.petercollingridge.co.uk/interactive-svg-components/draggable-svg-element

-- From mouse events we can read various properties (clientX, screenX, movementX)
-- https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent/movementX
-- Easiest would be to just use movementX!

#ifdef __GHCJS__
onE e n = VirtualDom.on n . contramapS e
  where
    contramapS f k x = k (f x)

mouseover  = onE Event "mouseover"
mouseout   = onE Event "mouseout"
-- Note: use over/out rather than enter/leave!
mouseenter = onE Event "mouseenter"
mouseleave = onE Event "mouseleave"
mouseup    = onE Event "mouseup"
mousedown  = onE Event "mousedown"
-- mousemove = onE Event "mousemove"


foreign import javascript unsafe "$1.movementX"
  movementX :: Event -> Double
foreign import javascript unsafe "$1.movementY"
  movementY :: Event -> Double
foreign import javascript unsafe "$1.screenX"
  screenX :: Event -> Double
foreign import javascript unsafe "$1.screenY"
  screenY :: Event -> Double
foreign import javascript unsafe "$1.offsetX"
  offsetX :: Event -> Double
foreign import javascript unsafe "$1.offsetY"
  offsetY :: Event -> Double
-- TODO right property to read?
#else
type Html = ()
data Event = Event ()
movementX :: Event -> Double
movementY :: Event -> Double
screenX :: Event -> Double
screenY :: Event -> Double
offsetX :: Event -> Double
offsetY :: Event -> Double
movementX = const 0
movementY = const 0
screenX = const 0
screenY = const 0
offsetX = const 0
offsetY = const 0
#endif

{-
TODO all initial arguments *could* be in S/B moinad
What is the most general interface?
-}

-- hoverable d = do
--   (overOut, overOuted :: Events Bool) <- newEvent
--   (state :: Signal Bool) <- stepperS False overOuted
--   let dWithHandlers = fmap ( addProperty (mouseover $ const $ overOut True)
--                            . addProperty (mouseout $ const $ overOut False)
--                            ) d
--   pure $ (fmap dWithHandlers state, state)


{-
Diffable is AffineSpace, except the Diff type is just a monoid (rather than an additive group).
I.e. the vectors/patches can not be inverted.

  (.-.) ~ diff
  (.+^) ~ patch

-}

class Monoid (Diff p) => Diffable p where
  type Diff p :: *
  diff :: p -> p -> Diff p
  patch :: p -> Diff p -> p

data MouseEv
  = MouseNone -- TODO should we have this?
  | MouseUp
  | MouseDown
  | MouseOver
  | MouseOut
  | MouseMovedInside -- TODO should we have this?
  deriving (Enum, Eq, Ord, Show, Read)

instance Monoid MouseEv where
  mempty = MouseNone
  mappend x y = x -- last event to happen as

data MouseState = MouseState { mouseInside :: !Bool, mouseDown :: !Bool } deriving (Eq, Ord, Show, Read)

instance Monoid MouseState where
  mempty = MouseState False False -- how do we know this?
  mappend x y = x -- ?

instance Diffable MouseState where
  type Diff MouseState = MouseEv
  diff (MouseState False d1) (MouseState True d2)  = MouseOut
  diff (MouseState True  d1) (MouseState False d2) = MouseOver
  diff (MouseState i1 False) (MouseState i2 True)  = MouseUp
  diff (MouseState i1 True)  (MouseState i2 False) = MouseDown
  diff (MouseState i1 d1)    (MouseState i2 d2)
    | i1 == i2 && d1 == d2 = MouseMovedInside
    | otherwise            = MouseNone

  patch (MouseState inside down) MouseUp   = MouseState inside False
  patch (MouseState inside down) MouseDown = MouseState inside True
  patch (MouseState inside down) MouseOver = MouseState True   down

  -- If mouse goes while button is still pressed, release
  patch (MouseState inside down) MouseOut  = MouseState False  False

  -- If the mosue is moved, we must be inside
  patch (MouseState inside down) MouseMovedInside = MouseState True   down

  patch x _ = x

{-
Most general way of creating a drawing with mosue interaction.

Mouse position is relative to the entire drawing area (TODO currently assumes origin is in TL corner).
Only event handlers sent to the given drawing a are processed (i.e. transparent areas are ignored).
-}
withMousePositionState :: (Signal MousePositionState -> Signal Drawing) -> FRP (Signal Drawing, Signal MousePositionState)
withMousePositionState drawingF = do
  (mouseS, mouseE :: Events MouseEv) <- newEvent
  (state :: Signal MouseState) <- accumS mempty (fmap (flip patch) mouseE)

  (mousePosS, mousePosE :: Events (P2 Double)) <- newEvent
  (pos :: Signal (P2 Double)) <- stepperS (P $ V2 0 0) (mousePosE)

  let (posState :: Signal MousePositionState) = liftA2 MousePositionState pos state
  let (dWithHandlers :: Signal MousePositionState -> Signal Drawing) = (fmap . fmap)
                           ( id
                           . addHandler "mouseover" (\ev -> let e = Event ev in mousePosS (P $ V2 (offsetX e) (offsetY e)) >> mouseS MouseOver)
                           . addHandler "mouseout"  (\ev -> let e = Event ev in mousePosS (P $ V2 (offsetX e) (offsetY e)) >> mouseS MouseOut)
                           . addHandler "mouseup"   (\ev -> let e = Event ev in mousePosS (P $ V2 (offsetX e) (offsetY e)) >> mouseS MouseUp)
                           . addHandler "mousedown" (\ev -> let e = Event ev in mousePosS (P $ V2 (offsetX e) (offsetY e)) >> mouseS MouseDown)
                           . addHandler "mousemove" (\ev -> let e = Event ev in mousePosS (P $ V2 (offsetX e) (offsetY e)) >> mouseS MouseMovedInside)
                           ) drawingF
  pure $ (dWithHandlers posState, posState)


{-
Like withMousePositionState, optimized for non-recursive case.
-}
withMousePositionStateNR :: Signal Drawing -> FRP (Signal Drawing, Signal MousePositionState)
withMousePositionStateNR drawingF = do
  (mouseS, mouseE :: Events MouseEv) <- newEvent
  (state :: Signal MouseState) <- accumS mempty (fmap (flip patch) mouseE)

  (mousePosS, mousePosE :: Events (P2 Double)) <- newEvent
  (pos :: Signal (P2 Double)) <- stepperS (P $ V2 0 0) (mousePosE)

  let (posState :: Signal MousePositionState) = liftA2 MousePositionState pos state
  let (dWithHandlers :: Signal Drawing) = fmap
                           ( id
                           . addHandler "mouseover" (\ev -> let e = Event ev in mousePosS (P $ V2 (offsetX e) (offsetY e)) >> mouseS MouseOver)
                           . addHandler "mouseout"  (\ev -> let e = Event ev in mousePosS (P $ V2 (offsetX e) (offsetY e)) >> mouseS MouseOut)
                           . addHandler "mouseup"   (\ev -> let e = Event ev in mousePosS (P $ V2 (offsetX e) (offsetY e)) >> mouseS MouseUp)
                           . addHandler "mousedown" (\ev -> let e = Event ev in mousePosS (P $ V2 (offsetX e) (offsetY e)) >> mouseS MouseDown)
                           . addHandler "mousemove" (\ev -> let e = Event ev in mousePosS (P $ V2 (offsetX e) (offsetY e)) >> mouseS MouseMovedInside)
                           ) drawingF
  pure $ (dWithHandlers, posState)

withMouseState :: (Signal MouseState -> Signal Drawing) -> FRP (Signal Drawing, Signal MouseState)
withMouseState d = fmap (second $ fmap mouseState) $ withMousePositionState (d . fmap mouseState)

{-|
Like MouseState, but include position.
-}
data MousePositionState = MousePositionState
  { mousePosition :: !(P2 Double)
  , mouseState    :: !MouseState
  }
  deriving (Eq, Ord, Show)

-- TODO this function, 1) without actually moving the drawing, 2) relative the drawing in question
-- withMousePositionState :: Signal Drawing -> FRP (Signal Drawing, Signal MousePositionState)
-- withMousePositionState d = do
--   (d2, p) <- nudgeable d
--   (d3, ms) <- withMouseState (const d2)
--   pure $ (d3, liftA2 MousePositionState p ms)



hoverable :: (Signal Bool -> Signal Drawing) -> FRP (Signal Drawing, Signal Bool)
hoverable d = fmap (second $ fmap mouseInside) $ withMouseState (d . fmap mouseInside)

hoverable_ :: (Signal Bool -> Signal Drawing) -> FRP (Signal Drawing)
hoverable_ = fmap fst . hoverable

-- TODO implement in terms of (poss. opt version of withMouseState)
nudgeableDraggable :: Behavior Bool -> Signal Drawing -> FRP (Signal Drawing, Signal (P2 Double))
nudgeableDraggable allowMove d =  do
  (move, moved1 :: Events (V2 Double)) <- newEvent
  let moved = filterJust $ snapshotWith (\allow m -> if allow then Just m else Nothing) allowMove moved1
  (pos :: Signal (P2 Double)) <- accumS (P $ V2 0 0) (fmap (^+.) moved)

  let (dWithHandlers :: Signal Drawing) = fmap (addHandler "mousemove" $ handleMouseMove move) d

  pure $ ( liftA2 (\(P v) dWithHandlers' -> translate v dWithHandlers') pos dWithHandlers, pos )
  where
    (^+.) = flip (.+^)
    handleMouseMove dest ev1 = let ev = Event ev1 in
      dest $ V2 (movementX ev) (-movementY ev)

{-
Make something "nudgeable".
I.e. it responds to all mousemove events by moving in the same direction.
-}
nudgeable :: Signal Drawing -> FRP (Signal Drawing, Signal (P2 Double))
nudgeable = nudgeableDraggable (pure True)
-- Note: Could be optimized by copying nudgeableDraggable and removing the filtering altogether

nudgeable_ :: Signal Drawing -> FRP (Signal Drawing)
nudgeable_ = fmap fst . nudgeable

{-
Make something "draggable".

I.e. it responds to mousemove events by moving in the same direction iff the object is "grabbed".
An object becomes grabbed whenever it recieves a mousedown event and retains that property
until it recieves a mousedown or mouseup event.
-}
draggable :: Signal Drawing -> FRP (Signal Drawing, Signal (P2 Double))
draggable d = do
  (d2, mouseStateS :: Signal MouseState) <- withMouseState (const d)
  nudgeableDraggable (fmap mouseDown $ current $ mouseStateS) d2

draggable_ :: Signal Drawing -> FRP (Signal Drawing)
draggable_ = fmap fst . draggable



{-\
Represents one of a series of actions used to input a rectangle.

A rectangle is considered to have been described whenever:

- An EndRect event is emitted
- The most recent event, excepting all ContinueRect events, is a BeginRect event
  (and thus not an AbortRect) event.
-}
data RectInputEvent
  -- Begin creating a square from this point
  = BeginRect (P2 Double)
  -- Continue creating a square
  | ContinueRect (P2 Double)
  -- Finish creating square
  | EndRect (P2 Double)
  -- Abort current square in process
  | AbortRect
  deriving (Eq, Ord, Show)

{-
A rectangle, represented as two points.
-}
data Rect = Rect (P2 Double) (P2 Double)
  deriving (Eq, Ord, Show)

rectToTransform :: Rect -> Drawing -> Drawing
rectToTransform (Rect (P (V2 x1 y1)) (P (V2 x2 y2))) d = id
    $ Lubeck.Drawing.translateX x1
    $ Lubeck.Drawing.translateY y1
    $ Lubeck.Drawing.scaleX     (x2-x1)
    $ Lubeck.Drawing.scaleY     (y2-y1)
    $ d

{-Finished rects-}
squares :: Events RectInputEvent -> FRP (Events Rect)
squares = fmap (filterJust . fmap snd) . foldpE f (Nothing, Nothing)
  where
    f (BeginRect p1)   _            = (Just p1, Nothing)
    f (EndRect p2)     (Just p1, _) = (Nothing, Just (Rect p1 p2))
    f AbortRect        _            = (Nothing, Nothing)
    f (ContinueRect _) (x, _)       = (x,       Nothing)

{-Rects in the making. TODO-}
squaresTemp :: Events RectInputEvent -> FRP (Signal (Maybe Rect))
squaresTemp ri = do
  rs <- squaresTemp2 ri
  stepperS Nothing rs
squaresTemp2 :: Events RectInputEvent -> FRP (Events (Maybe Rect))
squaresTemp2 = fmap (fmap snd) . foldpE f (Nothing, Nothing)
  where
    f (BeginRect p1)   _             = (Just p1, Just (Rect p1 p1))
    f (ContinueRect p2) (Just p1, _) = (Just p1, Just (Rect p1 p2))
    f _ _ = (Nothing, Nothing)

mpsToRectInput :: P2 Double -> MouseEv -> RectInputEvent
mpsToRectInput pos MouseDown        = BeginRect pos
mpsToRectInput pos MouseUp          = EndRect pos
mpsToRectInput pos MouseOut         = AbortRect
mpsToRectInput pos _                = ContinueRect pos
-- mpsToRectInput pos MouseP

emittedAndIntermediateRectangles :: Signal MousePositionState -> FRP (Events Rect, Signal (Maybe Rect))
emittedAndIntermediateRectangles bigTransparentMPS = do
  let (mouseMove :: Events MousePositionState) = (updates bigTransparentMPS)
  mouseMove2 <- withPreviousWith (\e1 e2 -> mpsToRectInput (mousePosition e2) (mouseState e2 `diff` mouseState e1)) mouseMove
  finishedRects <- squares mouseMove2
  intermediateRects <- squaresTemp mouseMove2
  pure (finishedRects, intermediateRects)

{-
A drag overlay interface.

Whenever the given bool signal is False, behaves like 'opts'.

Otherwise, this displays an invisible drawing across the original image, that registers mouse events
and displays feedback on drag events. Whenever a drag action is completed, the rectangle in which
it was performed is sent.
-}
dragRect
  :: Signal Bool
  -> FRP (Signal Drawing, Events Rect)
dragRect activeS = do
  -- TODO could use a more efficient version
  (bigTransparentWithHandlers, bigTransparentMPS :: Signal MousePositionState)
    <- withMousePositionStateNR (pure bigTransparent)
  let overlay :: Signal Drawing = liftA2 (\bt s -> if s then bt else mempty) bigTransparentWithHandlers activeS
  (finishedRects, intermediateRects) <- emittedAndIntermediateRectangles bigTransparentMPS
  let (dragRectD :: Signal Drawing) = fmap dragRect intermediateRects
  pure $ (overlay <> dragRectD, finishedRects)
  where
    dragRect Nothing = mempty
    dragRect (Just rect) =
      fillColorA (Colors.blue `withOpacity` 0.3) $ rectToTransform rect $ align BL $ square

    bigTransparent =
      -- Test strange alignment
      -- align BR $
      -- TODO no color
        fillColorA (Colors.green `withOpacity` 0.1) $ Lubeck.Drawing.scale 3000 square

#ifdef __GHCJS__
{-
Displays a pair or plus/minus-labeled buttons.
-}
plusMinus :: Str -> Double -> FRP (Signal Html, Signal Double)
plusMinus label init = do
  (view, alter :: Events (Double -> Double)) <- componentEvent id (multiButtonWidget [] [("-", (/ 1.1)), ("+", (* 1.1))]) mempty
  zoom <- accumS init alter
  pure (mconcat [pure $ text (toJSString label), view], zoom)


onOff :: Str -> Bool -> FRP (Signal Html, Signal Bool)
onOff label init = do
  (view, alter :: Events Bool) <- componentEvent init selectEnumBoundedWidget mempty
  state <- stepperS init alter
  pure (mconcat [pure $ text (toJSString label), view], state)
-- selectEnumBoundedWidget :: (Eq a, Enum a, Bounded a, Show a) => Widget' a
#endif

----------

type SDrawing  = Signal Drawing
type SRDrawing = Signal RDrawing

-- renderDrawingTrace msg d = trace msg $ renderDrawing mempty d
renderDrawingTrace msg d = renderDrawing mempty d



main :: IO ()
main = do
#ifdef __GHCJS__
  setupEmit
  setupEmit2
  setupRender
  setupRender2

  -- retainMain
  (view0, zoomActive) <- onOff "Zoom active" False
  (view1, zoomX) <- plusMinus "Zoom X" 1
  (view2, zoomY) <- plusMinus "Zoom Y" 1
  let zoomXY = liftA2 V2 zoomX zoomY
#else
  let zoomActive = pure True :: Signal Bool
  let zoomXY = pure $ V2 1 1
#endif
  -- Debug
  when debugHandlers $ void $
    subscribeEvent (updates zoomXY) print

  let !(plotSD :: Styled Drawing) = trace "> plotSD" $ drawPlot $ mconcat
          [ plot (zip [1..10::Double] [31,35,78,23,9,92,53,71,53,42::Double])
              [x <~ _1, y <~ _2]
              (mconcat [line, fill, pointG])
          , plotLabel "(4,50)" [(4::Double, 50::Double)]
              [x <~ _1, y <~ _2]
          , plotLabel "(7,65)" [(7::Double, 65::Double)]
              [x <~ _1, y <~ _2]
          ]

  let (plotVD :: SDrawing) = fmap (\currentZoom -> (getStyled plotSD (zoom .~ currentZoom $ mempty))) zoomXY
  -- let plotD = getStyled plotSD mempty :: Drawing
  (plotSD :: SDrawing) <- draggable_ $ plotVD
  (plotSD2 :: SDrawing) <- draggable_ $ plotVD
  (plotSD3 :: SDrawing) <- draggable_ $ plotVD
  (plotSD4 :: SDrawing) <- draggable_ $ plotVD

  let purpleCircle = trace "> purpleCircle" $ Lubeck.Drawing.fillColorA (Colors.purple `withOpacity` 0.2) $ Lubeck.Drawing.scale 190 circle
  let pinkCircle   = trace "> pinkCircle" $ Lubeck.Drawing.fillColor Colors.pink $ Lubeck.Drawing.scale 150 circle
  let !redSquare    = trace "> redSquare" $ Lubeck.Drawing.fillColor Colors.red $ Lubeck.Drawing.scale 190 square
  let !blueSquare   = trace "> blueSquare" $ Lubeck.Drawing.fillColor Colors.blue $ Lubeck.Drawing.scale 190 square

  dc1 <- draggable_ $ pure (pinkCircle ||| redSquare)
  -- dc2 <- draggable_ $ pure purpleCircle

  let (sqs  :: Signal Bool -> SDrawing) = (fmap $ \t -> if t then blueSquare else redSquare)

  (sqs2 :: SRDrawing) <- (draggable_ <=< hoverable_) sqs >>= strictifyS . fmap (renderDrawing2 opts)
  (sqs3 :: SRDrawing) <- (draggable_ <=< hoverable_) sqs >>= strictifyS . fmap (renderDrawing2 opts)
  (sqs4 :: SRDrawing) <- (draggable_ <=< hoverable_) sqs >>= strictifyS . fmap (renderDrawing2 opts)
  (sqs5 :: SRDrawing) <- (draggable_ <=< hoverable_) sqs >>= strictifyS . fmap (renderDrawing2 opts)
  (sqs6 :: SRDrawing) <- (draggable_ <=< hoverable_) sqs >>= strictifyS . fmap (renderDrawing2 opts)
  (sqs7 :: SRDrawing) <- (draggable_ <=< hoverable_) sqs >>= strictifyS . fmap (renderDrawing2 opts)
  (sqs8 :: SRDrawing) <- (draggable_ <=< hoverable_) sqs >>= strictifyS . fmap (renderDrawing2 opts)
  -- (sqsb :: SDrawing) <- hoverable_ (fmap $ \t -> if t then blueSquare else redSquare)
  -- sqs2b <- draggable_ $ fmap (Lubeck.Drawing.scale 0.5) sqsb

  (dr, _) <- dragRect zoomActive

  (srds :: [SRDrawing]) <- mapM strictifyS $
  -- let (srds :: [SRDrawing]) =
              -- fmap (fmap $ renderDrawing opts)
                [ fmap (renderDrawingTrace "R Circle and square") dc1
                -- , fmap (renderDrawingTrace "R Zoom") dr
                -- , fmap (renderDrawingTrace "Hoverable square") sqs2
                , sqs2
                , fmap (renderDrawingTrace "R Plot") plotSD
                , fmap (renderDrawingTrace "Plot2") plotSD2
                , fmap (renderDrawingTrace "Plot3") plotSD3
                , fmap (renderDrawingTrace "R Circles") $ pure $ duplicateN 10 (V2 1 1) purpleCircle
                , fmap (renderDrawingTrace "Plot4") plotSD4
                ]
  -- let (sd :: SRDrawing) = mconcat srds
  (sd :: SRDrawing) <- let [a,b,c,d,e,f,g] = srds in do
    x <- fastMconcatS3 a b c
    y <- fastMconcatS3 d e f
    fastMconcatS3 x y f


  -- foo <- fastMconcatS3
  --                   sqs2
  --                   sqs3
  --                   sqs4
  -- bar <- fastMconcatS3
  --                   sqs5
  --                   sqs6
  --                   sqs7
  -- sd <- fastMconcatS3 foo bar sqs8
#ifdef __GHCJS__
  let allS =
            -- mconcat [view0, view1, view2,
            fmap (\x -> {-trace "E" $-} emitDrawing2 opts x) sd
            -- ]

  -- runAppReactive $ allS
  -- allS2 <- strictifyS allS
  let allS2 = allS
  runWithAnimation $ (allS2 :: Signal Html)
  print "Done!"
#else
  let allS = fmap (\x -> {-trace "E" $-} emitDrawing2 opts x) sd
  subscribeEvent (updates allS) $ \_ -> print "New image"
  return ()
#endif
  where
    !opts = mempty { dimensions = P (V2 1600 800) }

-- -- | Evaluates events passing through to WHNF before propagating
-- strictify :: Events a -> FRP (Events a)
-- strictify e = do
--   (s,e2) <- newEvent
--   subscribeEvent e $ \x -> seq x (s x)
--   return e2
--
-- -- | Evaluates events passing through to WHNF before propagating
-- strictifyS :: Signal a -> FRP (Signal a)
-- strictifyS s = do
--   !z <- pollBehavior (current s)
--   u2 <- strictify (updates s)
--   stepperS z u2
--

emitDrawing2 opts x = unsafePerformIO $ do
  -- beginEmit
  let !r = emitDrawing opts x
  -- endEmit
  pure r
renderDrawing2 opts x = unsafePerformIO $ do
  -- beginRender
  let !r = renderDrawing opts x
  -- endRender
  pure r

foreign import javascript safe "window.beginEmit = function(){ for (i = 0; i < 100000; i++){ i = i } }" setupEmit :: IO ()
foreign import javascript safe "window.endEmit = function(){ for (i = 0; i < 100000; i++){ i = i } }" setupEmit2 :: IO ()
foreign import javascript unsafe "window.beginEmit()" beginEmit :: IO ()
foreign import javascript unsafe "window.endEmit()" endEmit :: IO ()

foreign import javascript safe "window.beginRender = function(){ for (i = 0; i < 100000; i++){ i = i } }" setupRender :: IO ()
foreign import javascript safe "window.endRender = function(){ for (i = 0; i < 100000; i++){ i = i } }" setupRender2 :: IO ()
foreign import javascript unsafe "window.beginRender()" beginRender :: IO ()
foreign import javascript unsafe "window.endRender()" endRender :: IO ()

-- MISC test

duplicateN :: Int -> V2 Double -> Drawing -> Drawing
duplicateN n v d = mconcat $ take n $ iterate (translate v) d

-- The classic Photosphop "duplicate" function
duplicate :: Drawing -> Drawing
duplicate = duplicateAt (V2 50 50)

duplicateAt :: V2 Double -> Drawing -> Drawing
duplicateAt v d = d <> translate v d




-- animate' :: JSFun (IO ()) -> IO ()

#ifdef __GHCJS__
foreign import javascript unsafe
  "var req = window.requestAnimationFrame;   var f = function() { $1(); req(f); };   req(f);"
  animate' :: Callback (IO ()) -> IO ()

-- foreign import javascript unsafe "h$mainZCZCMainzimain();"
  -- retainMain :: IO ()

animate :: IO () -> IO ()
animate k = do
  cb <- syncCallback ThrowWouldBlock k
  animate' cb

data WithCount a = WithCount !Int a

{-|
Sample and render using requestAnimationFrame (forever).
-}
runWithAnimation :: Signal Html -> IO ()
runWithAnimation b = do
  (s, start) <- animated2 b
  runAppReactive s
  print "Starting animation"
  start


{-|
Similar to animated, but eliminate double rendering.
Only actually propagates when input has propagated.
-}
animated2 :: Signal a -> FRP (Signal a, IO ())
animated2 b = do
  let hB = current b
  z <- pollBehavior hB
  countB <- counter (updates b)

  let (cAndCountB) = liftA2 WithCount countB hB
    -- cAndCountB :: Behavior (WithCount a)

  (sink, ev) <- newEvent
  s <- stepperS z ev
  last <- newIORef (-1)
  -- forkIO $ forever $ do
  --   lastV <- readIORef last
  --   threadDelay 500000
  --   print lastV
  let start = animate $ do
            lastV <- readIORef last
            (WithCount n x) <- pollBehavior cAndCountB
            -- TODO prevent re-render
            when (n /= lastV) $
              sink (seq x x)
            writeIORef last n
            -- print (n, lastV)
  return (s, start)

{-|
Browser only: Poll the given behavior from the animation callback.
Returns a signal that will be pushed to when invoked from the ACB and an action to start animation.
-}
animated :: Behavior a -> FRP (Signal a, IO ())
animated b = do
  z <- pollBehavior b
  (sink, ev) <- newEvent
  s <- stepperS z ev
  let start = animate $ do
            x <- pollBehavior b
            sink x
  return (s, start)
#endif

-- #else
-- main = print "Dummy"
-- #endif
