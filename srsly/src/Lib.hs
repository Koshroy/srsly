{-# LANGUAGE NoImplicitPrelude #-}

module Lib
    ( someFunc
    , startGui
    , mainLoop

    , module Base
    ) where

import Base
import Common

import CardTree

import qualified Control.Monad.Trans.State.Lazy as ST


someFunc :: IO ()
someFunc = putStrLn $ pack "someFunc"


startGui :: IO Bool
startGui = do
  initMsg <- initGUI
  case null initMsg of
    True -> return True
    False -> do
      putStrLn $ pack $ mconcat initMsg
      return False


mainLoop :: IO ()
mainLoop = do
  ctx <- newGtkCtx
  tree <- getDirectoryCardTree "/home/koushik/decks/example_deck/categories"
  let actions = do
        changeCardTree tree
        cb <- printTreeSelectionCB
        setTreeViewSelectFunc cb
        showGtkCtx
  ST.evalStateT actions ctx
  mainGUI
  return ()
