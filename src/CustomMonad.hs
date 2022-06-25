{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

-- |
module CustomMonad (someFunc) where

import Control.Applicative
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

newtype Prob a = Prob {getProb :: [(a, Rational)]} deriving (Show)

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map f xs
  where
    f (Prob inner, p) = map (\(x, r) -> (x, p * r)) inner

instance Functor (Prob) where
  fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs

instance Applicative (Prob) where
  pure x = Prob [(x, 1 % 1)]
  (<*>) = ap

instance Monad (Prob) where
  return x = Prob [(x, 1 % 1)]
  p1 >>= p2 = flatten (fmap p2 p1)

data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads, 1 % 2), (Tails, 1 % 2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads, 1 % 10), (Tails, 9 % 10)]

flipThree :: Prob Bool
flipThree = do
  a <- coin
  b <- coin
  c <- loadedCoin
  return (all (== Tails) [a, b, c])

someFunc :: IO ()
someFunc = do
  print $ liftM (+ 1) (Just 8)
  print $ ap (Just (+ 1)) (Just 4)
  print $ liftM (+ 1) [1, 2, 3, 4]
  print $ ap [(+ 1), (+ 2)] [1, 2, 3, 4]
  print $ getProb flipThree
