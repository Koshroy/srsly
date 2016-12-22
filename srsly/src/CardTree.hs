{-# LANGUAGE NoImplicitPrelude #-}

module CardTree
  ( 
  ) where

import Base
import Common

import qualified Control.Monad.Trans.State.Lazy as ST


changeCardTree :: CardTree -> GtkState ()
changeCardTree cards = do
  ctx <- ST.get
  let ts = treeStore ctx
  liftIO $ treeStoreClear ts
  liftIO $ treeStoreInsertForest ts [] 0 cards
  changeTreeStore ts
  

