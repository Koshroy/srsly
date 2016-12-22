{-# LANGUAGE NoImplicitPrelude #-}

module Lib
    ( someFunc
    , startGui
    , mainLoop

    , module Base
    ) where

import Base

import GtkCtx

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
  ctx <- getGtkCtx
  ST.evalStateT showGtkCtx ctx
  mainGUI
  return ()
