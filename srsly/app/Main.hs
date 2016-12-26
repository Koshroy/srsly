{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

main :: IO ()
main = do
  success <- startGui
  case success of
    True -> mainLoop
    False -> return ()
  
