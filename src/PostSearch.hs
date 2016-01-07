
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections #-}

module Main where

import Prelude hiding (div)
import qualified Prelude

import GHCJS.Types(JSString, jsval)
import GHCJS.VDOM.Event (click, change, submit, stopPropagation, preventDefault, value)
import GHCJS.VDOM.Element (p, h1, div, text, form, button, img, hr, custom)
import GHCJS.VDOM.Attribute (src, width, class_, href, target, width, src)

import Lubeck.FRP
import Lubeck.App (Html, runApp, runAppStatic)

import BD.Data.Account (Account)
import qualified BD.Data.Account as A
import BD.Data.SearchPost (SearchPost)
import qualified BD.Data.SearchPost as P
import BD.Query.PostQuery
import Lubeck.Forms (Widget, Widget')

-- IO (Behavior Html, Behavior SimplePostQuery)     state of post search form: def/user
-- Event ()                                         submit: user

-- State of results
-- B Html, B [SearchPost]                           resulting posts: def/HTTP result

-- Event ImageId                                    add to library: user



postSearchResult :: Widget (List Post) ()
postSearchResult _ posts = div () [
    h1 [] [text "Search Results"]
    , div [] [text $ "Found " ++ toString (List.length posts) ++ " posts"]
    , postTable () posts
  ]

postTable :: Widget [Post] ()
postTable _ posts = table [class_ "table table-striped table-hover"]
  [ tbody [] $ List.map (\xs -> tr [] (List.map (postTableCell ()) xs)) (divide 5 posts)]

postTableCell :: Widget Post ()
postTableCell _ post = let
  -- userName always defined when returned from result page
  userName = case post.userName of Just x -> x
  in td []
  [ a [ target "_blank",
        href $ Maybe.withDefault post.url post.igWebUrl, class_ "hh-brighten-image"]
      [ imgFromWidthAndUrl' 150 post.thumbnailUrl [fixMissingImage] ],
    div () [
      a [href "#"
      ] [text $ "@" ++ userName]
      ],
    div () [text $ "(l) " ++ showWithThousandSeparator post.likeCount],
    div () [text $ "(c) " ++ showWithThousandSeparator post.commentCount]
    ]


-- MAIN

main :: IO ()
main = runAppStatic page


-- UTILITY

-- @divide n @ separates a list into sublists of length n.
-- The last chunk may be shorter.
divide : Int -> List a -> List (List a)
divide n xs = case xs of
  [] -> []
  xs -> List.take n xs :: divide n (List.drop n xs)

imgFromWidthAndUrl' _ _ = div () [text "Img"]
-- imgFromWidthAndUrl : Int -> String -> Html
-- imgFromWidthAndUrl       width url = img [width width, src url] []
--
-- imgFromWidthAndUrl' : Int -> String -> List Html.Attribute -> Html
-- imgFromWidthAndUrl' width url attrs = img (attrs ++ [width width, src url]) []
--
-- imgFromWidthAndUrlCircle : Int -> String -> Html
-- imgFromWidthAndUrlCircle width url = img [class_ "img-circle", width width, src url] []
--
-- imgFromWidthAndUrlCircle' : Int -> String -> List Html.Attribute -> Html
-- imgFromWidthAndUrlCircle' width url attrs = img (attrs ++ [class_ "img-circle", width width, src url]) []

showWithThousandSeparator :: Int -> JSString
showWithThousandSeparator = const "100,000"
-- showWithThousandSeparator n = String.fromList $ List.concat $ List.intersperse (String.toList ",") $ divideFromEnd 3 $ String.toList (toString n)
