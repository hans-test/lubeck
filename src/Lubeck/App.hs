
{-# LANGUAGE GeneralizedNewtypeDeriving, QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

{-|
This module provides a way to write single-page apps using "Lubeck.FRP" and "Web.VirtualDom".
All functions in this module are intended to be bound to @main@.

The basic entry-points are 'runAppStatic', which renders a static page, and 'runAppReactive', which renders a reactive page.
Typically the reactive HTML will be generated by composing several other reactive values, representing the state of the app.
To get interactivity, add event listeners to the generated HTML. These can be fed back into the network using events (see "Lubeck.FRP"
for details).
-}
module Lubeck.App
    ( module Lubeck.Html
    -- * Standard
    , runAppStatic
    , runAppReactive
    -- ** With keyboard-events
    , runAppReactiveX
    , KbdEvents(..)

    -- * Elm-style
    , runApp
    , runAppPure
    ) where
import Prelude hiding (div)
import qualified Prelude

import Control.Concurrent (threadDelay, forkIO, myThreadId)
import Control.Monad (forM_, forever, unless)
import Control.Monad.STM (atomically)
import Control.Exception (catch, SomeException)
import qualified Control.Concurrent.STM.TChan as TChan
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Data.List
import Data.Monoid
import Data.Maybe(fromMaybe)
import Data.Default (def)

import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Web.VirtualDom as VD
import GHCJS.Concurrent(isThreadSynchronous, isThreadContinueAsync)
import GHCJS.Foreign.Callback

import qualified Web.VirtualDom.Html.Events as DE


-- import GHCJS.VDOM (mount, diff, patch, VNode, DOMNode)
-- import GHCJS.VDOM.Event (initEventDelegation)

import qualified Web.VirtualDom as VD

import GHCJS.Foreign.QQ (js, jsu, jsu')
import GHCJS.Types(JSString, jsval, JSVal)

import Lubeck.FRP
import Lubeck.Html
import Lubeck.Util (which)

data KbdEvents = Key Int deriving (Show)

foreign import javascript unsafe "document.addEventListener('keyup', $1);"
  js_JSFunListener :: (Callback (JSVal -> IO ())) -> IO ()

jsFunListener :: (Callback (JSVal -> IO ())) -> IO ()
jsFunListener cb = js_JSFunListener cb

foreign import javascript unsafe "(function() { /* console.log($1); */ return $1; })()"
  makeDamnEvent :: JSVal -> DE.Event

kbdListener :: (JSVal -> IO()) -> IO ()
kbdListener handler = do
    callback <- asyncCallback1 handler -- synchronously?
    jsFunListener  callback


-- |
-- Run an application a static HTML page. The page is rendered to the DOM immediately.
runAppStatic :: Html -> IO ()
runAppStatic x = runAppReactive (pure x)

-- |
-- Run an application a static HTML page. The page is rendered to the DOM immediately, and
-- subsequently whenever the signal is updated.
--
runAppReactive :: Signal Html -> IO ()
runAppReactive s = runAppReactiveX (s, Nothing)

runAppReactiveX :: (Signal Html, Maybe (Sink KbdEvents)) -> IO ()
runAppReactiveX (s, mbKbdSink) = flip catch (\e -> print (e :: SomeException)) $ do
  -- VD = Virtual DOM, RD = Real DOM

  -- print "Setting up first VD"
  -- showThreadInfo

  initVD <- pollBehavior $ current s
  initRD <- VD.createElement initVD
  varVD <- newIORef initVD
  varRD <- newIORef initRD

  VD.appendToBody initRD

  -- may include all kinds of UI input: mouse pos, remote terminal input etc
  case mbKbdSink of
    Just kbdSink -> kbdListener $ \e -> kbdSink $ Key (which . makeDamnEvent $ e)
    Nothing      -> pure ()

  subscribeEvent (updates s) $ \newVD -> do
    -- print "Updating VD"
    -- showThreadInfo

    prevVD <- readIORef varVD
    prevRD <- readIORef varRD

    delta <- VD.diff prevVD newVD
    newRD <- VD.patch prevRD delta

    writeIORef varVD newVD
    writeIORef varRD newRD
    return ()
  return ()
  where
    showThreadInfo = do
      putStrLn (replicate 50 '-')
      putStrLn . ("myThreadId:            " ++) . show =<< myThreadId
      putStrLn . ("isThreadSynchronous:   " ++) . show =<< isThreadSynchronous =<< myThreadId
      putStrLn . ("isThreadContinueAsync: " ++) . show =<< isThreadContinueAsync =<< myThreadId


{-# DEPRECATED runAppPure "Please use runAppStatic or runAppReactive" #-}
runAppPure
  :: (Events action -> IO (Behavior model))
  -> (Sink action -> model -> Html)
  -> IO ()
runAppPure update render = runApp (fmap (fmap $ \x -> (x,Nothing)) . update) render

{-# DEPRECATED runApp "Please use runAppStatic or runAppReactive" #-}
-- | Run an application, Elm-style.
runApp
  :: (Events action -> IO (Behavior (model, Maybe (IO action))))
  -> (Sink action -> model -> Html)
  -> IO ()
runApp update render = do
  -- Setup chans/vars to hook into the FRP system

  -- Actions to run (from user or finished jobs)
  frpIn      <- (TChan.newTChanIO :: IO (TChan.TChan action))
  -- Fired whenever state has been updated
  frpUpdated <- (TChan.newTChanIO :: IO (TChan.TChan ()))
  -- Current state
  -- Should not be read before frpUpdated has been emitted at least once
  frpState   <- (TVar.newTVarIO (error "Should not be sampled") :: IO (TVar.TVar model))
  -- Jobs for worked thread
  frpJobs    <- (TChan.newTChanIO :: IO (TChan.TChan (IO action)))

  -- Compile FRP system
  forkIO $ do
    system <- runFRP' update
    -- Propagate initial value (or we won't see anything)
    (frpSystemState system) (\(st, _) -> atomically $ TVar.writeTVar frpState st >> TChan.writeTChan frpUpdated ())
    -- Register output
    (frpSystemOutput system) $ \(st, job) -> do
        atomically $ TVar.writeTVar frpState st
        case job of
            Nothing -> return ()
            Just job -> atomically $ TChan.writeTChan frpJobs job
        atomically $ TChan.writeTChan frpUpdated ()
    forever $ do
      i <- atomically $ TChan.readTChan frpIn
      -- putStrLn $ "Processing event: " ++ show i
      (frpSystemInput system) i

  -- Job thread
  forkIO $
    forever $ do
      job <- atomically $ TChan.readTChan frpJobs
      -- putStrLn "Starting a job"
      res <- job
      -- putStrLn "Job finished"
      atomically $ TChan.writeTChan frpIn res

  -- Enter rendering loop on main thread
  do
    -- initEventDelegation []
    renderingLoop $ do
      atomically $ TChan.readTChan frpUpdated
      st <- atomically $ TVar.readTVar frpState
      return $ render (atomically . TChan.writeTChan frpIn) st

  where
    -- Repeatedly call the given function to produce a VDOM, then patch it into the given DOM node.
    renderingLoop :: IO VD.Node -> IO ()
    renderingLoop = VD.renderingLoop VD.appendToBody
