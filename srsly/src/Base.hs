{-# LANGUAGE NoImplicitPrelude #-}

module Base
    ( module ClassyPrelude
    , module Data.Tree
    , module Graphics.UI.Gtk
    , module G
    ) where

import ClassyPrelude
import Data.Tree

import Graphics.UI.Gtk hiding (Builder, on, toBuilder)
import qualified Graphics.UI.Gtk as G (Builder, on, toBuilder)
