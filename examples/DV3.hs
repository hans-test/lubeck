
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, OverloadedStrings, TupleSections #-}

module Main where

import Prelude hiding (div)
import qualified Prelude

import Data.Monoid ((<>))

import GHCJS.Types(JSString, jsval)
import qualified Web.VirtualDom.Html as H
import qualified Data.JSString

import qualified Data.List
import qualified Data.Ord
import System.Random (mkStdGen, randoms, split)

import Control.Lens (view)
import Control.Lens.Operators


-- TODO move
import Data.Time (UTCTime(..), DiffTime, Day(..))


-- TODO Debug
import Control.Concurrent(forkIO, threadDelay)
import Control.Monad(forever)

import Lubeck.FRP
import Lubeck.DV.Drawing
import Lubeck.DV.SimpleNormalized
import Lubeck.App (Html, runAppReactive)
import Lubeck.Forms
import Lubeck.Forms.Select
  -- (Widget, Widget', component, bothWidget)
import Lubeck.Forms.Basic
import Lubeck.Drawing
import Lubeck.Util(showJS, parseDateAndTimeToUTC)

import Linear.Vector
import Linear.Affine
import Linear.V0
import Linear.V1
import Linear.V2
import Linear.V3
import Linear.V4

import qualified Lubeck.Drawing

import Data.Colour (withOpacity)
import qualified Data.Colour.Names as Colors


-- MAIN

-- | Invariant behavior/signal.
--
-- Acts exactly like a behavior, except that all state changes are guarded by the given predicate.
-- If an update would cause the behavior to invalidate the invariant, this is ignored.
--
-- Consequently : ib === invariant p b ==> fmap p (behavior ib) === pure True

-- data Invariant f a = IB (a -> Bool) (f a)
--
-- invariant :: (a -> Bool) -> f a -> Invariant f a
-- invariant = IB
--
-- invariant :: Invariant f a -> f a
-- behavior (IB _ b) = b

class HasInvariant a where
  inv :: a -> Bool

accumInvariantB :: HasInvariant a => a -> Events (a -> a) -> IO (Behavior a)
accumInvariantB z e = accumB z $ fmap (\f -> toId (predToMaybe inv . f)) e
  where
    toId :: (a -> Maybe a) -> a -> a
    toId f x = case f x of
      Nothing -> x
      Just x2 -> x2
    predToMaybe :: (a -> Bool) -> a -> Maybe a
    predToMaybe p x = if p x then Just x else Nothing

newtype Pos = Pos (Double,Double)
instance HasInvariant Pos where
  inv (Pos (x,y)) = ins (0,1) x && ins (0,1) y
    where
      ins (a,b) c = a <= b && b <= c



chooseDrawing :: [Drawing] -> IO (Signal Html)
chooseDrawing ds = do
  (view, intE) <- componentEvent 0 (rangeWidget 0 (length ds - 1) 1) mempty
  drawingS <- stepperS mempty (fmap (ds !!) intE)
  return $ mconcat [view, (fmap (toSvg rendOpts) drawingS)]
  where
    rendOpts  = mempty
                { originPlacement = Center
                , dimensions      = P $ V2 1000 500
                }
    -- backgroundGrid = scale 600 xyCoords

main :: IO ()
main = do
  dS <- chooseDrawing $ fmap (scale 10 . (<> scale 10 xyCoords))
    [
    ]
  runAppReactive $ fmap (H.text "Please choose a graph:" <>) dS
  where
    rotateVector a = transformVector (rotation a)

    redCircle   = scale 10 $ fillColorA (Colors.red `withOpacity` 0.4) circle
    blueCircle  = scale 10 $ fillColorA (Colors.blue `withOpacity` 0.4) circle
    greenCircle = scale 10 $ fillColorA (Colors.green `withOpacity` 0.4) circle
    plotStyle = id
      $ renderingRectangle  .~ V2 500 250
      $ linePlotStrokeColor .~ (Colors.blue  `withOpacity` 0.5)
      $ barPlotBarColors    .~ cycle [Colors.purple `withOpacity` 0.5]
      $ mempty

    ps           = zipWith _p rand1 rand2

    -- combine [f, g...] x = mconcat [f x, g x...]
    combine fs x = mconcat $ fmap ($ x) fs

    headOnly xs = if null xs then [] else [head xs]
    lastOnly xs = if null xs then [] else [last xs]
    _p x y = P (V2 x y)


-- Some random series for testing

randPoints, ordRandPoints :: [P2 Double]
ordRandPoints = (Data.List.sortBy (Data.Ord.comparing (view _x)) $ take 10 randPoints)
randPoints    = zipWith (\x y -> P (V2 x y)) rand1 rand2
randVectors   = zipWith (\x y -> (V2 x y)) rand1 rand2

rand1, rand2 :: [Double]
rand1 = randoms $ fst $ split randG
rand2 = randoms $ snd $ split randG

randG = (mkStdGen 8712261455)
-- randG = (mkStdGen 123456789)
-- randG = (mkStdGen 3141599999)