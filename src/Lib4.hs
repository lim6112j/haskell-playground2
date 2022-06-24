{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib4 where

import Control.Monad.State (State, runState, state)

type Stack = [Int]

--newtype State s a = State {runState :: s -> (a, s)}
--
--instance Functor (State s) where
--fmap f (State g) = State (\s -> (f a, s))
--where
--(a, s) = g s
--
--instance Applicative (State s) where
--pure a = State (\s -> (a, s))
--State s1 <*> State s2 = State (\s -> (f a, ss))
--where
--(f, s) = s1 s
--(a, ss) = s2 s
--
--instance Monad (State s) where
--return a = State (\s -> (a, s))
--s1 >>= f = State (\s -> (b, s))
--where
--(a, s) = runState s1 s
--(b, ss) = runState (f a) ss
--type State s a = StateT s Identity a

pop :: State Stack Int
pop = state $ \(x : xs) -> (x, xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((), a : xs)

stackManip :: State Stack Int
stackManip = do
  push 3
  a <- pop
  pop

someFunc :: IO ()
someFunc = do
  let (a, s) = runState stackManip [1, 2, 3, 4]
  print (a, s)
