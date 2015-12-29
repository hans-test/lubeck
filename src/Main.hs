
{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, TemplateHaskell, OverloadedStrings, TupleSections #-}

import Prelude hiding (div)
import qualified Prelude

import Control.Applicative
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forM_, forever, unless)
import Data.String (fromString)
import qualified Data.Map as Map
import Data.Map(Map)
import Control.Monad.STM (atomically)
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Data.List
import Data.Monoid
import Data.Maybe(fromMaybe)
import Data.Default (def)
import Control.Lens (over, set)
import Control.Lens.TH(makeLenses)

--import Data.JSString.Text

import GHCJS.VDOM.Event (click, change, submit, stopPropagation, preventDefault, value)
import GHCJS.Foreign.QQ (js, jsu, jsu')
import GHCJS.Types(JSString, jsval)
import GHCJS.VDOM.Element (p, h1, div, text, form, button, img, hr, custom)
import GHCJS.VDOM.Attribute (src, width, class_)
import qualified GHCJS.VDOM.Element as E
import qualified GHCJS.VDOM.Attribute as A
import GHCJS.VDOM.Unsafe (unsafeToAttributes, Attributes')
import Data.JSString.Text (textFromJSString)

import FRP2
import App

import qualified BD.Data.Account as A
import qualified BD.Data.AdCampaign as AC
import qualified BD.Data.Count as C
import qualified BD.Data.SearchPost as P
import BD.Data.SearchPost(SearchPost)
import BD.Data.Interaction
import BD.Types


type Widget i o = Sink o -> i -> Html

type Account = A.Account

username :: Account -> Text
username = A.username

data Action
  = LoginGo
  | Logout
  | Pure (Model -> Model)
  | GotUser Account
  | Then Action Action
  
--  | GotCampaigns [Campaign]
--  | GoToCampaign Int

-- For debugging only
instance Show Action where
  show = g where
    g LoginGo         = "LoginGo"
    g Logout         = "Logout"
    g (GotUser _) = "GotUser"
    g (Pure _) = "Pure"
    g (Then _ _) = "Then"

data Model = NotLoggedIn { _loginPage :: LoginPage}
           | LoadingUser
           | AsUser { _user :: A.Account
                    , _userModel :: UserModel }

data ViewSection = UserView -- | CampaignsImageLibrary | Campaign Int

data LoginPage = LoginPage { _loginUsername :: JSString
                           , _loginPass :: JSString }

data UserModel = UserModel { _campaigns :: [AC.AdCampaign]
                           , _viewSection :: ViewSection }

makeLenses ''Model
makeLenses ''LoginPage
makeLenses ''UserModel

update :: E Action -> IO (R (Model, Maybe (IO Action)))
update = foldpR step initial
  where
    initial = (NotLoggedIn (LoginPage "forbestravelguide" "bar"), Nothing)

    step LoginGo              (NotLoggedIn lp,_) = (LoadingUser, Just $ loginUser lp)
    step LoginGo              (m,_) = (m, Nothing)
    step (Pure f)             (m,_) = (f m, Nothing)
    step Logout               (_,_) = initial
    step (GotUser acc)        (_,_) = (AsUser acc (UserModel [] UserView), Just $ getCampaigns acc)

--    step (LoadAction a b)     (model,_) = (model,Just $ fmap ReplaceModel (loadShoutouts a b))
--    step (ReplaceModel model) (_,_)     = (model,Nothing)

render :: Widget Model Action
render sink LoadingUser = text "Loading User"
render sink (NotLoggedIn lp) = loginPageW sink lp
render sink (AsUser acc (UserModel camps UserView)) = div
  (customAttrs $ Map.fromList [("style", "width: 600px; margin-left: auto; margin-right: auto") ])
  [ h1 () [text "Hello"]
  , div ()
    [text $ A.username acc ]
  , div ()
    [text $ showJS $ A.latest_count acc ]
  , div ()
    [ text "number of campaigns: "
    , text $ showJS (length camps)]
  ]

loginPageW :: Widget LoginPage Action
loginPageW sink (LoginPage u pw) = form
  [ submit $ \e -> preventDefault e >> return () ]
  [
    -- E.input [ change $ \e -> [jsu|console.log(`e)|] ] [text "abc"]
  -- ,
    E.input [A.value u,
             change $ \e -> preventDefault e >> sink (Pure (set (loginPage . loginUsername) (value e)))] ()
   , button (click $ \_ -> sink LoginGo) [text "Login"] ]

loginUser :: LoginPage -> IO Action
loginUser (LoginPage s _) = do
  u <- A.getUser s
  return $ GotUser u

getCampaigns :: A.Account -> IO Action
getCampaigns acc = do
  cs <- AC.getUserCampaigns $ A.username acc
  return $ Pure $ set (userModel . campaigns) cs

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

-- | A limitation in ghcjs-vdom means that non-standard attributes aren't always defined properly.
-- This works around the issue. The value returned here should take the place of the standard
-- attribute definition, i.e. instead of (div () ..) or (div [..] ..), use (div (customAttrs []) ..).
--
-- If possible, use the functions exported by GHCJS.VDOM.Attribute instead.
customAttrs :: Map String String -> Attributes'
customAttrs attrs = let str = (fromString $ ("{"++) $ (++"}") $ drop 2 $ Map.foldWithKey (\k v s -> s++", "++show k++":"++show v) "" attrs) :: JSString
  in unsafeToAttributes [jsu'| {attributes:JSON.parse(`str)} |]
