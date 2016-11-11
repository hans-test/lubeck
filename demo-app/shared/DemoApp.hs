
{-
A demo application using Lubeck libraries.
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

module DemoApp where

import BasePrelude
import Lubeck.FRP
import Lubeck.Drawing
import Lubeck.Str
import qualified Data.Colour.Names as Colors

-- Shared types here...
type Foo = Int
