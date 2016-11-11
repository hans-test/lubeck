
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

#ifdef __GHCJS__
-- =============================================================================
-- Client
-- =============================================================================
import Lubeck.App(runAppReactive)
import Lubeck.Html
import Web.VirtualDom as VD
import Web.VirtualDom.Html as VD
import Web.VirtualDom.Html.Attributes as VD
import GHCJS.Foreign.Callback as CB
-- import GHCJS.Marshal(toJSVal) -- only need a function to convert Aeson(Value) to JsVal
import GHCJS.Types(JSVal, JSString)
import Lubeck.Drawing.Internal.Backend.FastRenderer
#else
-- =============================================================================
-- Server
-- =============================================================================
#endif





type Foo = Int





#ifdef __GHCJS__
-- =============================================================================
-- Client
-- =============================================================================


init _ _ r = do
  pure ()
update _ _ _ _ = do
  pure ()
render _ _ = do
  pure ()

main = do
  print (123 :: Foo)
  cb <- asyncCallback1 $ \canvasDomNode -> do
        print "Done setup"
        runRenderingLoopOn (DOMCanvasElement canvasDomNode) init update render
        -- creatingCanvasDoneU (DOMCanvasElement canvasDomNode)
  let mainV = pure $
          VD.staticNode "div" [VD.id "wrap-canvas"] $
            pure $ VD.staticNodeWithCB cb "canvas"
              [ VD.attribute "width" "800"
              , VD.attribute "height" "400"
              , VD.attribute "id" "canvas"
              ] []
  runAppReactive $ pure mainV


#else
-- =============================================================================
-- Server
-- =============================================================================



main = print (123 :: Foo)

#endif
