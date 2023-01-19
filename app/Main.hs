{-# LANGUAGE OverloadedStrings #-}
module Main where

import CustomMonad
import Lib5
import TextShow
main :: IO ()
main = do
  let example = Example 3 4
  print $ showt example
  
