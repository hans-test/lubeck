
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections,
  TemplateHaskell, ConstraintKinds, CPP #-}

-- |
-- Basics for drawing plots.
--
-- TODO nice docs
--
-- Essentially:
--
-- * This modlules draws basic static, animated and interactive graphics
--   For static graphics, use 'Styled', for animatedinteractive, use 'StyledT Behavior' or similar.
--
-- * Looks are provided by the 'Styled' monad. Use lens API to modify styling attributes.
--
-- * TODO substyling
--
-- * Data is generally recieved in R^n. Do normalization and axes  elsewhere.
--
-- This module should NOT handle:
--
--  * Data normalization (it expects everything in the unit hypercube)
--
--  * Fancy overloading (all data is in concrete types: arguments to functions below)
--
--  * Styling (everything parameterized on the style record)
--
--    ** Whatever the value of this the data will be rendered "correctly" (if not "intelligibly")
module Lubeck.DV.Drawing
  (
    Str(..)

  -- * Drawing data

  -- $normalizeInputPoint
  -- $normalizeInputScalar

  -- ** Scatter
  , scatterData
  , scatterDataX
  , scatterDataY

  -- ** Lines
  , lineData
  , stepData
  , linearData

  -- ** Bars and sizes
  , barData
  , barData2
  , barData3
  , barData4

  , barDataWithColor
  , barDataWithColor2
  , barDataWithColor3

  , circleData
  , circleDataWithColor
  , pieChartData

  -- | Ratios
  , ratioData
  , ratioDataWithColor

  -- ** Discrete data and counts
  , discreteData
  , discreteDataGroup
  , intData
  , discreteHeatMap

  , treeMapGraph
  , treeMapGraphWithColor


  -- * Drawing axes
  , ticks
  , ticksNoFilter
  , barPlotTicks

  -- * Drawing axes
  , labeledAxis

  -- * Drawing legends
  , quantLegend
  , intervalLegend
  , scaleLegend

  -- * Drawing titles
  , title

  -- * Drawing overlays/explanatories
  , overlay

  -- * Utility
  , plotRectangle
  ) where

import Prelude hiding (div)
import qualified Prelude

import Control.Applicative
import Control.Lens ()
import Control.Lens.Operators
import Control.Lens.TH (makeLenses)
import Control.Monad.Identity
import Control.Monad.Reader
-- import Data.AffineSpace
import Data.Colour (Colour, AlphaColour, withOpacity, blend)
import Data.Monoid
import Data.Map(Map)
-- import Data.VectorSpace
import qualified Data.Colour.Names as Colors
-- import qualified Data.VectorSpace as VS

import Linear.Vector
import Linear.Affine
-- import Linear.Matrix hiding (translation)
-- import Linear.Metric -- Needed?
import Linear.V0
import Linear.V1
import Linear.V2
import Linear.V3
import Linear.V4

import Lubeck.Drawing
import Lubeck.DV.Styling
import qualified Lubeck.Drawing

-- TODO consolidate origin, intoRect

-- | Draw data for a scatter plot.
--
--   X and Y coordinates map to points in the plotting rectangle.
--
--   Can be combined with `scatterDataX`, `scatterDataY` and `lineData`.
scatterData :: (Monad m) => [P2 Double] -> StyledT m ()
scatterData ps = do
  style <- ask
  let base  = id
            $ fillColorA (style^.scatterPlotFillColor)
            $ strokeColorA (style^.scatterPlotStrokeColor)
            $ scale (style^.scatterPlotSize) circle
  let origin = P $ V2 0 0
  let intoRect = transformPoint (scalingX (style^.renderingRectangle._x) <> scalingY (style^.renderingRectangle._y))
  draw $ mconcat $ fmap (\p -> translate (p .-. origin) base) (fmap intoRect ps)
  return ()

-- | Draw data for a scatter plot, ignoring Y values.
--
--   By default the X coordinate of each given point is rendered as a vertical line at the corresponding position
--   in the plotting rectangle.
--
--   Can be combined with `scatterData`, `lineData` etc.
scatterDataX :: (Monad m) => [P2 Double] -> StyledT m ()
scatterDataX ps = do
  style <- ask
  let base = strokeColorA (style^.scatterPlotStrokeColor) $ strokeWidth 1.5 $ translateY 0.5 $ verticalLine
  let origin = P $ V2 0 0
  let intoRect = transformPoint (scalingX (style^.renderingRectangle._x) <> scalingY (style^.renderingRectangle._y))
  draw $ mconcat $ fmap (\p -> scaleY (style^.renderingRectangle._y) $ translateX (p^._x) base) (fmap intoRect ps)
  return ()

-- | Draw data for a scatter plot ignoring X values.
--
--   By default the X coordinate of each given point is rendered as a horizontal line at the corresponding position
--   in the plotting rectangle.
--
--   Can be combined with `scatterData`, `lineData` etc.
scatterDataY :: (Monad m) => [P2 Double] ->  StyledT m Drawing
scatterDataY ps = do
  style <- ask
  let base = strokeColorA (style^.scatterPlotStrokeColor) $ strokeWidth 1.5 $ translateX 0.5 $ horizontalLine
  let origin = P $ V2 0 0
  let intoRect = transformPoint (scalingX (style^.renderingRectangle._x) <> scalingY (style^.renderingRectangle._y))
  return $ mconcat $ fmap (\p -> scaleX (style^.renderingRectangle._x) $ translateY (p^._y) base) (fmap intoRect ps)

-- | Draw data for a line plot.
--
--   Can be combined with `scatterData`, `scatterDataX` etc.
lineData :: (Monad m) => [P2 Double] -> StyledT m Drawing
lineData []     = mempty
lineData [_]    = mempty
lineData (p:ps) = do
  style <- ask
  let lineStyle = id
                . strokeColorA  (style^.linePlotStrokeColor)
                . fillColorA    (style^.linePlotFillColor)
                . strokeWidth   (style^.linePlotStrokeWidth)
  let origin = P $ V2 0 0
  let intoRect = transformPoint (scalingX (style^.renderingRectangle._x) <> scalingY (style^.renderingRectangle._y))
  return $ translate (intoRect p .-. origin) $ lineStyle $ segments $ betweenPoints $ fmap intoRect (p:ps)


-- | Draw a step chart.
--
-- Similar to 'lineData', except it renders a series of changes (vectors) relative a starting point.
--
-- Can be combined with `scatterData`, `scatterDataX` etc.
--
-- See /Visualize this/, p. 124
stepData :: (Monad m) => P2 Double -> [V2 Double] -> StyledT m Drawing
stepData z vs = lineData (offsetVectors z vs)

-- | Draw a linear function @ax + b@. Renders the function in the [0..1] domain,
--   i.e to get a line intersecting the outer ends of the X and Y axis use @linearData (-1) 1@.
--
--   Can be combined with `scatterData`, `scatterDataX` etc.
linearData :: (Monad m) => Double -> Double -> StyledT m Drawing
linearData a b = lineData $ fmap (\x -> P $ x `V2` f x) [0,1]
  where
    f x = a*x + b

-- TODO bar graphs can be transposed (x/y) (how?)

-- | Draw a one-dimensional bar graph.
--
-- For grouped/stacked charts, see `barData2`, `barData3` etc.
barData :: (Monad m) => [P1 Double] -> StyledT m Drawing
barData ps = do
  style <- ask
  let barWidth = 1/fromIntegral (length ps + 1)
  let barFullOffset = barWidth + barWidth * (style^.barPlotUngroupedOffset._x)
  let base = alignB $ fillColorA ((style^.barPlotBarColors) !! 0) $ square
  return $ scaleX (2/3) $ scaleRR style $ mconcat $ zipWith (\n -> translateX (n * barFullOffset)) [1..] $
    fmap (\(P (V1 v)) -> scaleX barWidth $ scaleY v $ base) ps
  where
    alignB = translate (V2 0 0.5)
    scaleRR = transform . scalingRR
    scalingRR style = let r = style^.renderingRectangle in scaling (r^._x) (r^._y)

-- | Draw a two-dimensional bar graph.
--
--  Every dimension in the given data set maps to a seprate bar.
--
-- Bars can be redered as grouped (default), stacked, or on opposite sides of the axis, depending on styling.
barData2 :: (Monad m) => [P2 Double] -> StyledT m Drawing

-- | Draw a three-dimensional bar graph.
--
--  Every dimension in the given data set maps to a seprate bar.
--
-- Bars can be redered as grouped (default) or stacked depending on styling.
barData3 :: (Monad m) => [P3 Double] -> StyledT m Drawing

-- | Draw a four-dimensional bar graph.
--
--  Every dimension in the given data set maps to a seprate bar.
--
-- Bars can be redered as grouped (default) or stacked depending on styling.
barData4 :: (Monad m) => [P4 Double] -> StyledT m Drawing
[barData2, barData3, barData4] = undefined

-- rawBarData4 :: [[R]] -> StyledT m Drawing


-- | Draw a bar graph.
barDataWithColor  :: (Monad m) => [P2 Double] -> StyledT m Drawing
-- | Draw a bar graph.
barDataWithColor2 :: (Monad m) => [P3 Double] -> StyledT m Drawing
-- | Draw a bar graph.
barDataWithColor3 :: (Monad m) => [P4 Double] -> StyledT m Drawing
-- | Draw
-- barDataWithColor4 :: [R5] -> StyledT m Drawing
[barDataWithColor, barDataWithColor2, barDataWithColor3] = undefined

-- | Visualizes a discrete count.
--
-- By default, this renders as a set of squares, coloured and grouped by the given element count.
--
-- See /Visualize this/ p XXII (Godfather example)
discreteData :: (Enum a, Monad m) => [(a, Int)] -> StyledT m Drawing
discreteData = undefined

-- | Similar to 'discreteData', but groups and counts equal elements in the given list.
--
-- >>> discreteDataGroup [True,True,False,True]
--
discreteDataGroup :: (Ord a, Monad m) => [a] -> StyledT m Drawing
discreteDataGroup = undefined

-- TODO calendar map, see Visualize this p70

-- | Discrete 2D heat map
-- See "Visualize this, p 233"
-- heatDiscrete2D :: (Enum a, Enum b) => (a -> b -> Double)

-- | Visualizes an integer.
intData :: (Monad m) => P1 Int -> StyledT m Drawing
intData (P (V1 n)) = discreteDataGroup (replicate n ())

-- | Visualizes a ratio. Similar to 'barData', but only shows a single element.
--
-- See http://webbddatascience.demo.aspnetzero.com/Application#/tenant/dashboard
ratioData :: (Monad m) => P1 Double -> StyledT m Drawing
ratioData (P (V1 v)) = do
  style <- ask
  let fg = style^.ratioPlotForegroundColor
  let bg = style^.ratioPlotBackgroundColor
  return $ transform (scalingRR style) (fillColorA fg (scaleY v (alignBL square)) <> fillColorA bg (alignBL square))
  where
    -- TODO move
    alignBL = translate (V2 0.5 0.5)
    scalingRR style = let r = style^.renderingRectangle in scaling (r^._x) (r^._y)


-- | Visualizes ratio with colour.
--
-- a la http://webbddatascience.demo.aspnetzero.com/Application#/tenant/dashboard
ratioDataWithColor :: (Monad m) => P2 Double -> StyledT m Drawing
ratioDataWithColor (P (V2 v1 v2)) = do
  style <- ask
  let bg  = style^.ratioPlotBackgroundColor
  let fg1 = style^.heatMapColour1
  let fg2 = style^.heatMapColour2
  let fg  = blend v2 fg1 fg2
  return $ transform (scalingRR style) (fillColorA fg (scaleY v1 (alignBL square)) <> fillColorA bg (alignBL square))
  where
    -- TODO move
    alignBL = translate (V2 0.5 0.5)
    scalingRR style = let r = style^.renderingRectangle in scaling (r^._x) (r^._y)

-- TODO consolidate ratioData, ratioDataWithColor


-- | Draws the plotting rectangle.
--
-- Useful for deugging.
plotRectangle :: (Monad m) => StyledT m Drawing
plotRectangle = do
  style <- ask
  return $ transform (scalingRR style) (scale 2 xyCoords)
  where
    scalingRR style = let r = style^.renderingRectangle in scaling (r^._x) (r^._y)

-- | Draw a circle size plot.
circleData :: (Monad m) => [P1 Double] -> StyledT m Drawing
circleData = undefined
-- TODO use a ratio/percantage type wrapper
-- TODO use area not radius

-- | Draw a pie chart.
pieChartData :: (Monad m) => [P1 Double] -> StyledT m Drawing
pieChartData = undefined
-- See https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Clipping_and_masking

-- | Draw
circleDataWithColor :: (Monad m) => [P2 Double] -> StyledT m Drawing
circleDataWithColor = undefined

-- circleDataWithColor = do
--   s <- getStyling
--   sizedData (baseCircleFromStyling c)
--   where
--     baseCircleFromStyling = ...

-- Higher order bar graphs.
-- Can render these by
-- color mapping + one of
--   stacking, grouping, alternating (same as grouping with no spacing), above/below (2 dimensions only)

--  Draw a bar graph.
-- barData2 :: [P2 Double] -> Styled Drawing
-- barData2 :: [P3 Double] -> Styled Drawing
-- barData2 :: [P4 Double] -> Styled Drawing

--  A size graph: scales the given objets and places them side by side.
-- sizedData :: [R] -> Styled Drawing -> Styled Drawing

-- | Draw a tree map.
treeMapGraph :: (Monad m) => [P1 Double] -> StyledT m Drawing
treeMapGraph = undefined
-- TODO use a ratio/percantage type wrapper


{-
Tree map like bottom one here:
https://infogr.am/link-building-strategies-from-the-experts
See also "Visualize this, p 157"
:
  Split horizontally, put 2 largest values to the left (split vertically), rest of values to the right by
  Split vertically, put 2 largest values at the top (split horizontally), rest of values at the bottom by
  etc

  (What to do if given an odd number of elements?)
-}


-- | Like 'treeMapGraph', mapping the last dimension to colour.
treeMapGraphWithColor :: (Monad m) => [P2 Double] -> StyledT m Drawing
treeMapGraphWithColor = undefined

-- | Draw a discrete heat map.
--
-- Example http://bokeh.pydata.org/en/latest/docs/gallery/les_mis.html
discreteHeatMap :: (Monad m) => [a] -> [b] -> (a -> b -> Double) -> StyledT m Drawing
discreteHeatMap = undefined
-- TODO use a ratio/percantage type wrapper




-- TODO Visualize pairs, lists, ordered sets, maps, trees, directed graphs
-- TODO Pie charts

-- | Basic type of legend.
-- Every string is matched to a single color.
--
-- >>> quantLegend ["Male", "Female"]
quantLegend :: [Str] -> StyledT m Drawing
quantLegend = undefined

-- | Similar to 'quantLegend', except strings indicate endpoint of a region.
-- The number of visualized
--
-- >>> quantLegend ["0", "50", "100"]
intervalLegend :: [Str] -> StyledT m Drawing
intervalLegend = undefined

--  | A legend where colours are interpolated in a continous space (i.e. for heat maps).
--
-- >>> scaleLegend (packStr . show) (0, 100)
scaleLegend :: (a -> Str) -> (a, a) -> StyledT m Drawing
scaleLegend = undefined

-- | Draw a title.
title :: Str -> StyledT m Drawing
title = undefined

-- | Draw a number of overlays which supports showing/hiding based on a current "focus".
overlay :: Map p Drawing -> m [p] -> StyledT m Drawing
overlay = undefined


-- TODO alternating tick size (i.e. every 50 year, 100 year etc)

-- TODO we should generalize this not to assume 2 axes
-- As far as we are concerned here there might be up to 4 axes (there may be more by overlaying)

-- TODO some creative tick positioning here
-- https://knowledge.infogr.am/featured

-- | Draw ticks.
-- Each argument is a list of tick positions (normalized to [0,1]) and an optional tick label.
-- Positions outside the normalized range are discarded.
ticks
  :: [(Double, Str)] -- ^ X axis ticks.
  -> [(Double, Str)] -- ^ Y axis ticks.
  -> Styled Drawing
ticks xt yt = ticksNoFilter (filterTicks xt) (filterTicks yt)
  where
    filterTicks = filter (withinNormRange . fst)
    withinNormRange x = 0 <= x && x <= 1

-- | Draw ticks.
-- Each argument is a list of tick positions (normalized to [0,1]) and an optional tick label.
-- Contrary to 'ticks', 'ticksNoFilter' accept ticks at arbitrary positions.
ticksNoFilter
  :: [(Double, Str)] -- ^ X axis ticks.
  -> [(Double, Str)] -- ^ Y axis ticks.
  -> Styled Drawing
ticksNoFilter xt yt = return $ mconcat [xTicks, yTicks]
  where
    xTicks = mconcat $ flip fmap xt $
      \(pos,str) -> translateX (pos * 300) $
        (scale kBasicTickLength $ strokeColor Colors.black $ strokeWidth 1.5 $ translateY (-0.5) verticalLine)
          <> (translateY (kBasicTickLength * (-1.5)) .rotate (turn*1/8)) (textEnd str)
    yTicks = mconcat $ flip fmap yt $
      \(pos,str) -> translateY (pos * 300) $
        (scale kBasicTickLength $ strokeColor Colors.black $ strokeWidth 1.5 $ translateX (-0.5) horizontalLine)
          <> (translateX (kBasicTickLength * (-1.5)) .rotate (turn*0.00001/8)) (textEnd str)

    kBasicTickLength = 10
    -- Note: Add infinitesimal slant to non-slanted text to get same anti-aliasing behavior
    -- kPositionTickRelAxis = (-0.5) -- (-0.5) for outside axis, 0 for centered around axis, 0.5 for inside
    -- kPositionLabelRelAxis = (-0.8) -- (kPositionTickRelAxis-0) to make label touch tick, (kPositionTickRelAxis-1) to offset by length of tick

barPlotTicks :: [Str] -> [Str] -> Styled Drawing
barPlotTicks = undefined


-- | Draw X and Y axis.
labeledAxis
  :: Str -- ^ X axis label.
  -> Str -- ^ Y axis label.
  -> Styled Drawing
labeledAxis labelX labelY = return $ mconcat
  [ scale 300 $ axis
  , translateY (300/2) $ translateX (-20) $ rotate (turn/4) $ textMiddle labelY
  , translateX (300/2) $ translateY (-20) $ textMiddle labelX]

axis, axisX, axisY :: Drawing
axis = mconcat [axisY, axisX]
axisX = strokeWidth 1.5 $ strokeColor Colors.black $ translateX 0.5 horizontalLine
axisY = strokeWidth 1.5 $ strokeColor Colors.black $ translateY 0.5 verticalLine

crossLineX, crossLineY :: Double -> Drawing
crossLineX n = translateX (n * 300) $ strokeWidth 2 $ strokeColor Colors.lightblue $ axisY
crossLineY n = translateY (n * 300) $ strokeWidth 2 $ strokeColor Colors.lightblue $ axisX





-- These are all low-level drawing functions.
--
-- Conventions:
--
--  * All data is normalized to fall inside the /unit hypercube/, meaning that each point in the data
--    set can be expressed as a linear combination of scalars in the range @[0..1]@.
--
--  * The position of each data points is mapped from the hypercube into the plotting rectangle
--    (currently hardcoded as @[(0,0)..(300,300])@).
--
--  * Returns data in the 'Styled' monad. Basic idea is that exctracting values from 'Styled'
--    may affect display (sometimes dramatically), but never the basic semantics of the data.
--
--  Consequently:
--
--  * Data must be normalized and labels, and the labels normalized in the same withOpacity
--    as the data before using this module.
--
--  * Plots generated by this module can be overlayd using the 'Drawing' monoid instance.
--
-- $normalizeInputScalar
-- Input should be normalized so that for each point @x@ in input, x ∈ [0,1].
--
-- $normalizeInputPoint
-- Input should be normalized so that for each point @Point x y@ in input, x ∈ [0,1], y ∈ [0,1].
--
--
