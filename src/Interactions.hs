
{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections #-}

module Main where

import Prelude hiding (div)
import qualified Prelude

import Control.Applicative
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forM_, forever, unless)
import Data.String (fromString)
import qualified Data.Map as Map
import Data.Map(Map)
import qualified Data.List
import Data.Monoid
import Data.Maybe(fromMaybe)
import Data.Default (def)
import Control.Lens (over, set)
import Control.Lens.TH(makeLenses)

import GHCJS.Foreign.QQ (js, jsu, jsu')
import GHCJS.Types(JSString, jsval)
import Web.VirtualDom.Html (p, h1, div, text, form, button, img, hr, a, table, tbody, th, tr, td, input, label)
import Web.VirtualDom.Html.Events (click, change, keyup, submit, stopPropagation, preventDefault, value)
import Web.VirtualDom.Html.Attributes (src, width, class_, href, target, width, src, style)
import qualified Web.VirtualDom.Html as E
import qualified Web.VirtualDom.Html.Attributes as A
import qualified Web.VirtualDom.Html.Events as Ev

import Lubeck.FRP
import Lubeck.App (Html, runApp)
import Lubeck.Forms (Widget, Widget')
import qualified Lubeck.Plots.Test as Plotting
import qualified Lubeck.Drawing as Drawing

import qualified BD.Data.Account as A
import qualified BD.Data.Count as C
import qualified BD.Data.SearchPost as P
import BD.Data.SearchPost(SearchPost)
import qualified BD.Data.Interaction as I
import BD.Data.Interaction hiding (interactions)

data Action
  = NoAction
  | LoadAction (Maybe JSString) (Maybe JSString)
  | ChangeModel (Model -> Model)
  | ReplaceModel Model -- (ReplaceModel x) == (ChangeModel (const x))

-- For debugging only
instance Show Action where
  show = g where
    g NoAction         = "NoAction"
    g (LoadAction _ _) = "LoadAction"
    g (ChangeModel _)  = "ChangeModel"
    g (ReplaceModel _) = "ReplaceModel"

data Model = Model
  { _requested    :: (Maybe JSString, Maybe JSString)
  , _interactions :: InteractionSet SearchPost }
makeLenses ''Model

update :: Events Action -> IO (Behavior (Model, Maybe (IO Action)))
update = foldpR step initial
  where
    initial = (Model (Nothing,Nothing) $ InteractionSet Nothing Nothing [], Nothing)

    step NoAction             (model,_) = (model,   Nothing)
    step (LoadAction a b)     (model,_) = (model,   Just $ do { so <- loadShoutouts a b ; return $ ChangeModel (set interactions so) })
    step (ReplaceModel model) (_,_)     = (model,   Nothing)
    step (ChangeModel f) (model,_)      = (f model, Nothing)

render :: Widget Model Action
render actions model = div
  -- (customAttrs $ Map.fromList [("style", "width: 900px; margin-left: auto; margin-right: auto") ])
  [ style "width: 900px; margin-left: auto; margin-right: auto" ]
  [ h1 [] [text "Shoutout browser"]
  , div []
    [buttonW actions (_requested model)]
  , div
    []
    [ interactionSetW actions (_interactions model) ]
  ]

buttonW :: Widget (Maybe JSString, Maybe JSString) Action
buttonW sink (x,y) = div
  []
  [ div [] $ pure $ E.input [A.value $ nToEmpty x, change $ \e -> preventDefault e >> sink (ChangeModel (set (requested._1) (emptyToN $ value e)))] []
  , div [] $ pure $ E.input [A.value $ nToEmpty y, change $ \e -> preventDefault e >> sink (ChangeModel (set (requested._2) (emptyToN $ value e)))] []
  , div [] $ pure $ button [click $ \e -> sink (LoadAction x y) >> preventDefault e] [text "Load shoutouts!"] ]
  where
    _1 f (x,y) = fmap (,y) $ f x
    _2 f (x,y) = fmap (x,) $ f y
    emptyToN "" = Nothing
    emptyToN xs = Just xs
    nToEmpty Nothing   = ""
    nToEmpty (Just xs) = xs

-- TODO make notice about how single-page "form" elements should not be inside forms (use div instead) - easiest way to get around auto-submit issues
-- TODO bug in ghcjs-vdom: both change and click return events that appear to accept stopPropagation but doesn't
doneEv x = stopPropagation x >> preventDefault x

interactionSetW :: Widget (InteractionSet SearchPost) Action
interactionSetW actions model = div []
  [ p [] [ text $ "Showing" <>  (fromMaybe "(anyone)" $ fmap ("@" <>) $ model .: from_account .:? A.username)
         , text $ " to "    <>  (fromMaybe "(anyone)" $ fmap ("@" <>) $ model .: to_account .:? A.username) ]
  , div [] (Data.List.intersperse (hr [] []) $ fmap (interactionW actions) $ model .: I.interactions)
  ]

interactionW :: Widget (Interaction SearchPost) Action
interactionW actions model = div []
  [ p [] [text (showJS $ model .: interaction_time)]
  -- Growth graph
  , div [class_ "row"]
    [
    ((Plotting.plotDrawingToSvg $
      (\x -> Drawing.stack [x,Drawing.xyAxis]) $ Plotting.drawDataPlot $ Plotting.basicDataGrowth id
        [ flip Drawing.Point 10   20
        , flip Drawing.Point 45  30
        , flip Drawing.Point 100 200
        , flip Drawing.Point 200 0
        ]))

      -- div [class_ "col-xs-8 col-lg-8"] [img [src greyImgUrl, width 600] []]
    , div [class_ "col-xs-4 col-lg-4"] [img [src (model .: medium .: P.url), width 200] []]
    ]
  , p [] [text "Estimated impact: (?)"]
  ]


-- MAIN

main :: IO ()
main = runApp update render


-- UTILITY

(.:)  :: a -> (a -> b) -> b
(.:)  x f = f x

(.:?) :: Maybe a -> (a -> b) -> Maybe b
(.:?) x f = fmap f x

showJS :: Show a => a -> JSString
showJS = fromString . show

-- A data URL representing a grey image
greyImgUrl :: JSString
greyImgUrl = "data:image/jpeg;base64,/9j/4AAQSkZJRgABAQAAAQABAAD/2wCEAAkGBxQSEhQUEhQUFBQUFBQUFBQUFBQUFBQUFBQXFxQUFBQYHCggGBwlHBQUITEhJSksLi4uFx8zODMsNygtLiwBCgoKDAwMDgwMDiwZFBksLCwsKywsLDc3Kyw3LCwsLDcsNzcsNyssLCwsLDc3LDcsLCwsLDcsNyw3NzcsNyw3LP/AABEIAOEA4QMBIgACEQEDEQH/xAAYAAEBAQEBAAAAAAAAAAAAAAAAAQIDB//EABkQAQEBAQEBAAAAAAAAAAAAAAABEQJBMf/EABUBAQEAAAAAAAAAAAAAAAAAAAAB/8QAFBEBAAAAAAAAAAAAAAAAAAAAAP/aAAwDAQACEQMRAD8A9QoqIpCCwRKjVTAIBgGqIAqKKCsiKmqCoEBEqiAWhQBFQDVRoEZrVSgzgqCurLWIIKkUBFQBUxQQi4AoEFQVBC1FMASBAEVJABQERSAAoIjTPQM6ADrWWkAVFACmgAAAAsEUBFqABFoJaACCgIilBAAWEIAJ0RKDI1gDdSrUAVFAAACAAACiAqKlAEAFEAAoIKgBQAWJFARUoICA6AgC0KBSIsAAAIRQVAASqlBAAVCKAlWoBAQBFQFixIArPSpQAQHSoqAsEXAAAAAFQBQQACAgAJjSRQTRYgFhUKCKigasTFBWbFS0EEAbqWrTAIuoAoAARYBAqSAqVQEouIAYQoJGkKAioCFKYAgtBGmWoAlq1mgmAgOqKgEVFAABRFADFBAQCoqAuloaAGgIKgCAAqAEWJABK0zQTEVAdUWoAqAKIoKJFALQASqzQAUEKAAQ0AwAEpqAtBAWBIAanQz1QTFAHWpWqyAACiKAAABBQEEChQAQFgqABUAAAQoC2oADNarNBFAHWotQUEAUAQWCQFEoCoqUCAQAAAQAoqAAkAAoEXA0GUqs0AMUHRKpUEgaKCooqpagIoIClEBRAAVAAQFEUEAACgAmroJUWs0BWdAdgqVFRYigqUAFSKqBEUAogoEKIAgLUVAFqKCQADEUBAAGVqAgKK6JV1lBSJQFBAWNRICAgqqBoAqURAAAAAANRagLpqYAi6lQFZtWsgirig2lBBFQBTkFVYAIiwABAVq+FARKAKCAi0vxAFKACIAVmqClZUEAEH//2Q=="

-- -- | A limitation in ghcjs-vdom means that non-standard attributes aren't always defined properly.
-- -- This works around the issue. The value returned here should take the place of the standard
-- -- attribute definition, i.e. instead of (div () ..) or (div [..] ..), use (div (customAttrs []) ..).
-- --
-- -- If possible, use the functions exported by GHCJS.VDOM.Attribute instead.
-- customAttrs :: Map String String -> Attributes'
-- customAttrs attrs = let str = (fromString $ ("{"++) $ (++"}") $ drop 2 $ Map.foldWithKey (\k v s -> s++", "++show k++":"++show v) "" attrs) :: JSString
--   in unsafeToAttributes [jsu'| {attributes:JSON.parse(`str)} |]
