
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections,
  TemplateHaskell, CPP #-}

-- |
-- Basics for drawing plots.
--
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


{-
ABSTRACTION BOUNDARIES

This module should NOT handle:

  - Data normalization (it expects everything in the unit hypercube)
  - Fancy overloading (all data is in concrete types: arguments to functions below)
  - Styling (everything parameterized on the style record)
    - Whatever the value of this the data will be rendered "correctly" (if not "intelligibly")


-}

module Lubeck.Plots.Drawing
    (
    -- * Drawing data
    --   $normalizeInputPoint
    --   $normalizeInputScalar
      scatterData
    , scatterDataX
    , scatterDataY
    , lineData
    , linearData
    , barData

    -- * Drawing axes
    , ticks
    , ticksNoFilter
    , labeledAxis
    -- ** Utilities
    , crossLineX
    , crossLineY

    -- * Drawing legends

    -- * Drawing titles

    -- * Drawing overlays/explanatories

    -- Line overlays, box plots, heat maps
    -- Stacking and graphing box plots
    -- Generating legends
    -- Generating proper axises
    -- Visualize pairs, lists, ordered sets, maps, trees, directed graphs
    -- Pie charts

    -- * Styling
    , Styling
    -- TODO all lenses here
    , renderingRectangle
    , linePlotStrokeColor
    , linePlotStrokeWidth
    , linePlotStrokeType

    , scatterPlotStrokeColor
    , scatterPlotFillColor
    , scatterPlotShape

    , barPlotBarColor
    , barPlotWidth
    , barPlotStandardOffset
    , barPlotGroupInternalOffset
    , barPlotSpaceUsed

    , Styled
    , getStyled
    , withDefaultStyle

    ) where

import Prelude hiding (div)
import qualified Prelude

import qualified Data.JSString
import GHCJS.Types(JSString, jsval)

import Data.Colour (Colour, AlphaColour, withOpacity)
import qualified Data.Colour.Names as Colors
import Data.Monoid ((<>), First(..))
import Control.Applicative
import Data.VectorSpace
import qualified Data.VectorSpace as VS
import Data.AffineSpace

import Control.Monad.Reader

import Control.Lens ()
import Control.Lens.TH (makeLenses)

-- import qualified Web.VirtualDom as VD
-- import qualified Web.VirtualDom.Html as H
-- import qualified Web.VirtualDom.Html.Attributes as H
-- import qualified Web.VirtualDom.Html.Events as H
-- import qualified Web.VirtualDom.Svg.Events as SvgEv

-- import Lubeck.FRP
-- import Lubeck.Forms
-- import Lubeck.Forms.Basic
import Lubeck.Drawing
import Lubeck.Util(showJS)
import qualified Lubeck.Drawing

data Styling = Styling
  { _dummy :: ()

  -- ^ Rectangle in which the plot will be rendered (default @300 x 300@)
  , _renderingRectangle :: First Vector

  -- Line plots
  , _linePlotStrokeColor :: AlphaColour Double
  , _linePlotStrokeWidth :: AlphaColour Double
  , _linePlotStrokeType  :: ()

  -- Scatter plots
    -- point size, fillColor, strokeColor, shape?
  , _scatterPlotStrokeColor :: AlphaColour Double
  , _scatterPlotFillColor   :: AlphaColour Double
  , _scatterPlotShape       :: ()

  -- Bar plots

  , _barPlotBarColor :: AlphaColour Double
  -- Bar width
  -- Default 1
  , _barPlotWidth :: First Vector
  -- Space between bars/bar groups (if used)
  -- Default 0.5
  , _barPlotStandardOffset :: First Vector
  -- Space between bars in the same group (if used)
  -- Default 0
  , _barPlotGroupInternalOffset :: First Vector
  -- Extra offset between bar groups (if used)

  -- Percentage of horizintal dim taken up by plots, in [0..1] (default 1)
  -- I.e. https://infogr.am/average_temperature_of_6_major_deserts
  , _barPlotSpaceUsed :: Double

  -- Color allocator
    -- TODO idea: to allocate colors to categories/dimensions
    -- I.e. when generating overlapping scatter plots, grouped bar graphs, AND or legends
    -- the color have a semantic purpose and thus should not be defined in styling (see above)
    -- To solve this, Styling provides an (infinite ?) supply of colors, and functions
    -- below can just use enumerations/categories as their arguments
    -- I.e.
      -- multiLineData :: Enum a => (a, [R2]) -> Styled Drawing
      -- stackedAndColoredBarData4 :: [R4] -> Styled Drawing
      -- stackedAndGroupedBarData4 :: [R4] -> Styled Drawing
      -- legend :: (Enum a, Show a) => [a] -> Styled Drawing

  -- Axis/ticks
    -- X,Y axis name
      -- NOTE: Standard styles: left/below centered, at end

    -- X Axis standard (usually left) line (strokeWith, strokeColorA)
    -- X Axis opposite                line (strokeWith, strokeColorA)
    -- Y Axis standard                line (strokeWith, strokeColorA)
    -- Y Axis opposite                line (strokeWith, strokeColorA)

    -- Axis arrow end?

    -- NOTE: strike-through/background ticks are rarely used together
    -- X,Y strike-through/background ticks
    -- X,Y tick (length, width, pos rel axis (see below), strokeColorA)
    -- Text position relative tick
    -- Text rotation
      -- NOTE: common(x,y) is (turn/4,0), (turn/8,0), (0,0)
  }
  deriving (Show)

makeLenses ''Styling

instance Monoid Styling where
  mempty = Styling
    { _dummy = mempty
    , _renderingRectangle = First $ Just $ Vector 300 300

    , _linePlotStrokeColor         = Colors.red `withOpacity` 0.6
    , _linePlotStrokeWidth         = Colors.red `withOpacity` 0.6
    , _linePlotStrokeType          = mempty

    , _scatterPlotStrokeColor      = Colors.red `withOpacity` 0.6
    , _scatterPlotFillColor        = Colors.red `withOpacity` 0.6
    , _scatterPlotShape            = mempty


    , _barPlotBarColor             = Colors.blue `withOpacity` 0.6
    , _barPlotWidth                = First $ Just $ Vector 0 1
    , _barPlotStandardOffset       = First $ Just $ Vector 0 0.5
    , _barPlotGroupInternalOffset  = First $ Just $ Vector 0 0
    , _barPlotSpaceUsed = 1

    }
  mappend = const


newtype Styled a = Styled { _getStyled :: Reader Styling a }
  deriving (Functor, Applicative, Monad, MonadReader Styling)

instance Monoid a => Monoid (Styled a) where
  mempty = pure mempty
  mappend = liftA2 mappend

-- | Extract a 'Styled' value.
getStyled :: Styled a -> Styling -> a
getStyled = runReader . _getStyled

withDefaultStyle :: Styled a -> a
withDefaultStyle x = getStyled x mempty

-- Pie charts
-- See https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Clipping_and_masking

-- | Draw data for a scatter plot.
scatterData :: [R2] -> Styled Drawing
scatterData ps = return $ scale 300 $ mconcat $ fmap (\p -> translate (p .-. origin) base) ps
  where
    base = fillColorA (Colors.red `withOpacity` 0.6) $ scale (10/300) circle
    origin = Point 0 0

-- | Draw data for a scatter plot ignoring Y values.
scatterDataX :: [R2] -> Styled Drawing
scatterDataX ps = return $ scale 300 $ mconcat $ fmap (\p -> translateX (x p) base) ps
  where
    base = strokeColorA (Colors.red `withOpacity` 0.6) $ strokeWidth 1.5 $ translateY 0.5 $ verticalLine
    origin = Point 0 0

-- | Draw data for a scatter plot ignoring X values.
scatterDataY :: [R2] -> Styled Drawing
scatterDataY ps = return $ scale 300 $ mconcat $ fmap (\p -> translateY (y p) base) ps
  where
    base = strokeColorA (Colors.red `withOpacity` 0.6) $ strokeWidth 1.5 $ translateX 0.5 $ horizontalLine
    origin = Point 0 0

-- | Draw data for a line plot.
lineData :: [R2] -> Styled Drawing
lineData []     = mempty
lineData [_]    = mempty
lineData (p:ps) = return $ scale 300 $ translate (p .-. origin) $ lineStyle $ segments $ betweenPoints $ (p:ps)
  where
    lineStyle = strokeColorA (Colors.red `withOpacity` 0.6) . fillColorA (Colors.black `withOpacity` 0) . strokeWidth 2.5
    origin = Point 0 0

-- Step chart, see Visualize this p 124
stepData :: R2 -> [V2] -> Styled Drawing
stepData z vs = lineData (offsetVectors z vs)

-- | Draw a linear function @ax + b@. Renders the function in the [0..1] domain,
--   i.e to get a line intersecting the outer ends of the X and Y axis use @linearData (-1) 1@.
linearData :: R -> R -> Styled Drawing
linearData a b = lineData $ fmap (\x -> x `Point` f x) [0,1]
  where
    f x = a*x + b

-- | Draw a bar graph.
barData :: [R] -> Styled Drawing
barData ps = return $ scale 300 $ mconcat $
    fmap (\p -> scaleX (1/fromIntegral (length ps)) $ scaleY p $ base) ps
  where
    -- TODO horizontal stacking (nicer with proper envelopes!)
    base = fillColorA (Colors.blue `withOpacity` 0.6) $ square

-- | Visualizes a count
-- See "Visualize this" pXXII (Godfather example)
-- discreteData :: Enum a => [(a, Int)] -> Styled Drawing

-- TODO calendar map, see Visualize this p70

-- | Discrete 2D heat map
-- See "Visualize this, p 233"
-- heatDiscrete2D :: (Enum a, Enum b) => (a -> b -> Double)


-- | Visualizes a ratio. Essentially a 1-category bar graph.
-- ratioData :: R -> Styled Drawing
-- a la http://webbddatascience.demo.aspnetzero.com/Application#/tenant/dashboard

-- TODO bar graphs can be transposed (x/y)

-- Higher order bar graphs.
-- Can render these by
-- color mapping + one of
--   stacking, grouping, alternating (same as grouping with no spacing), above/below (2 dimensions only)

-- | Draw a bar graph.
-- barData2 :: [R2] -> Styled Drawing
-- barData2 :: [R3] -> Styled Drawing
-- barData2 :: [R4] -> Styled Drawing


-- circleData :: [R] -> Styled Drawing
-- circleData = do
--   s <- getStyling
--   sizedData (baseCircleFromStyling c)
--   where
--     baseCircleFromStyling = ...

-- | A size graph: scales the given objets and places them side by side.
-- sizedData :: [R] -> Styled Drawing -> Styled Drawing


-- TODO tree map like bottom one here:
-- https://infogr.am/link-building-strategies-from-the-experts
-- See also "Visualize this, p 157"
{-
Algrorithm:
  Split horizontally, put 2 largest values to the left (split vertically), rest of values to the right by
  Split vertically, put 2 largest values at the top (split horizontally), rest of values at the bottom by
  etc

  (What to do if given an odd number of elements?)
-}

-- TODO alternating tick size (i.e. every 50 year, 100 year etc)

-- TODO we should generalize this not to assume 2 axes
-- As far as we are concerned here there might be up to 4 axes (there may be more by overlaying)

-- TODO some creative tick positioning here
-- https://knowledge.infogr.am/featured

-- | Draw ticks.
-- Each argument is a list of tick positions (normalized to [0,1]) and an optional tick label.
-- Positions outside the normalized range are discarded.
ticks
  :: [(Double, JSString)] -- ^ X axis ticks.
  -> [(Double, JSString)] -- ^ Y axis ticks.
  -> Styled Drawing
ticks xt yt = ticksNoFilter (filterTicks xt) (filterTicks yt)
  where
    filterTicks = filter (withinNormRange . fst)
    withinNormRange x = 0 <= x && x <= 1

-- | Draw ticks.
-- Each argument is a list of tick positions (normalized to [0,1]) and an optional tick label.
-- Contrary to 'ticks', 'ticksNoFilter' accept ticks at arbitrary positions.
ticksNoFilter
  :: [(Double, JSString)] -- ^ X axis ticks.
  -> [(Double, JSString)] -- ^ Y axis ticks.
  -> Styled Drawing
ticksNoFilter xt yt = return $ mconcat [xTicks, yTicks]
  where
    xTicks = mconcat $ flip fmap xt $
      \(pos,str) -> translateX (pos * 300) $
        (scale kBasicTickLength $ strokeColor Colors.black $ strokeWidth 1.5 $ translateY (-0.5) verticalLine) <> (translateY (kBasicTickLength * (-1.5)) .rotate (turn*1/8)) (textEnd str)
    yTicks = mconcat $ flip fmap yt $
      \(pos,str) -> translateY (pos * 300) $
        (scale kBasicTickLength $ strokeColor Colors.black $ strokeWidth 1.5 $ translateX (-0.5) horizontalLine) <> (translateX (kBasicTickLength * (-1.5)) .rotate (turn*0.00001/8)) (textEnd str)

    kBasicTickLength = 10
    -- Note: Add infinitesimal slant to non-slanted text to get same anti-aliasing behavior
    -- kPositionTickRelAxis = (-0.5) -- (-0.5) for outside axis, 0 for centered around axis, 0.5 for inside
    -- kPositionLabelRelAxis = (-0.8) -- (kPositionTickRelAxis-0) to make label touch tick, (kPositionTickRelAxis-1) to offset by length of tick

-- | Draw X and Y axis.
labeledAxis
  :: JSString -- ^ X axis label.
  -> JSString -- ^ Y axis label.
  -> Styled Drawing
labeledAxis labelX labelY = return $ mconcat
  [ scale 300 $ axis
  , translateY (300/2) $ translateX (-20) $ rotate (turn/4) $ textMiddle labelY
  , translateX (300/2) $ translateY (-20) $ textMiddle labelX]

axis = mconcat [axisY, axisX]
axisX = strokeWidth 1.5 $ strokeColor Colors.black $ translateX 0.5 horizontalLine
axisY = strokeWidth 1.5 $ strokeColor Colors.black $ translateY 0.5 verticalLine

crossLineX n = translateX (n * 300) $ strokeWidth 2 $ strokeColor Colors.lightblue $ axisY
crossLineY n = translateY (n * 300) $ strokeWidth 2 $ strokeColor Colors.lightblue $ axisX











-- Interactive
--
-- -- | Draw data for a scatter plot with an optional drawing pop-up.
-- scatterData :: [(R2, Maybe Drawing)] -> IO (Signal Drawing)
-- scatterData ps = scale 300 $ mconcat $ fmap (\p -> translate (p .-. origin) base) ps
--   where
--     base = fillColorA (Colors.red `withOpacity` 0.6) $ scale (10/300) circle
--     origin = R2 0 0
