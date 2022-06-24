{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
module Lib3 where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.List
import Control.Monad.Trans.State

pytaTriples :: ListT (State Bool) (Int, Int, Int)
pytaTriples = do
  x <- oneOf [1 .. 10]
  y <- take 1 [x .. 10]
  z <- take 1 [y .. 10]
  guard (x * x + y * y == z * z)
  return (x, y, z)
