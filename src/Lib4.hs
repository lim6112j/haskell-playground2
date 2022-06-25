module Lib4 where

type Stack = [Int]

newtype State s a = State {runState :: s -> (a, s)}

data Name = Name {firstName :: String, lastNmae :: String}

instance Functor (State s) where
  fmap f (State g) = State (\s -> (f a, s))
    where
      (a, s) = g s

instance Applicative (State s) where
  pure a = State (\s -> (a, s))

  State s1 <*> State s2 = State $ \s ->
    let (f, newState) = s1 s
        (a, newState2) = s2 newState
     in (f a, newState2)

instance Monad (State s) where
  return a = State (\s -> (a, s))

  --State h >>= f = State $ \s ->
  --let (a, newState) = h s
  --(State g) = f a
  --in g newState

  s1 >>= f = State $ \s ->
    let (a, newState) = runState s1 s
        (b, newState2) = runState (f a) newState
     in (b, newState2)

pop :: State Stack Int
pop = State $ \(x : xs) -> (x, xs)

push :: Int -> State Stack ()
push a = State $ \xs -> ((), a : xs)

stackManip :: State Stack Int
stackManip = do
  push 3
  a <- pop
  pop

stackStuff :: State Stack ()
stackStuff = do
  a <- pop
  if a == 5
    then push 5
    else do
      push 3
      push 8

get = State $ \s -> (s, s)

put newState = State $ \s -> ((), newState)

stackyStack :: State Stack ()
stackyStack =
  get
    >>= ( \x ->
            if x == [1, 2, 3]
              then put [1, 1, 1]
              else put [4, 3, 1]
        )

--stackyStack = do
--stackNow <- get
--if stackNow == [1, 2, 3]
--then put [8, 3, 1]
--else put [9, 1, 1]

someFunc :: IO ()
someFunc = do
  let (a, s) = runState stackManip [1, 2, 3, 4]
  print (a, s)
  let (a, s) = runState stackStuff [9, 5, 2, 0, 1]
  print (a, s)
  let val = runState stackyStack [1, 2, 3]
  print val
