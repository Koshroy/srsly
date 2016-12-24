{-# LANGUAGE NoImplicitPrelude #-}

module Common
  ( CardTree(..)

  , module GtkCtx
  ) where

import Base
import GtkCtx

type CardTree = Forest FilePath
