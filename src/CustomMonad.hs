-- |
module CustomMonad (someFunc) where

import Data.Functor
import Data.Ratio

liftM :: (Monad m) => (a -> b) -> m a -> m b
-- liftM f m = m <&> f
-- liftM f m = m >>= (\x -> return (f x))
liftM f m = do
  x <- m
  return (f x)

ap :: (Monad m) => m (a -> b) -> m a -> m b
--ap mf m = do
--f <- mf
--x <- m
--return (f x)
ap mf m = do
  f <- mf
  f <$> m

join :: (Monad m) => m (m a) -> m a
join m = do
  x <- m
  x

-- m >>= f  == join (fmap f m)

someFunc :: IO ()
someFunc = do
  print $ liftM (+ 1) (Just 8)
  print $ ap (Just (+ 1)) (Just 4)
  print $ liftM (+ 1) [1, 2, 3, 4]
  print $ ap [(+ 1), (+ 2)] [1, 2, 3, 4]
