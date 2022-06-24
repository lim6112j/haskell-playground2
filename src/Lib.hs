{-# LANGUAGE FlexibleContexts #-}
module Lib
    ( someFunc
    ) where
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
isBigGang::Int -> Bool
isBigGang n = n > 5

isBigGang'::Int -> (Bool, String)
isBigGang' n | n>5 = (True, "this is big gang")
             | otherwise = (False, "this is small gang")
isBigGang''::Int -> WriterT (Bool, String) IO ()
isBigGang'' n | n > 5 =
                  tell (True, "big gang")
              | otherwise =
                  tell (False, "small gang")
isBigGang'''::Int -> WriterT String IO Int
isBigGang''' n | n > 5 = do
                   tell "how big is this gang? \n"
                   WriterT $ return (n, "Big Gang")
               | otherwise = do
                   tell "how big is this gang? \n"
                   WriterT $ return (n, "Small Gang")
myName::String -> ReaderT String IO String
myName step = do
  name <- ask
  return (step ++ ", I am " ++ name)
localExample :: ReaderT String IO (String, String, String)
localExample = do
  x <- myName "first"
  y <- local (++ "dy") (myName "second")
  z <- myName "third"
  return (x, y, z)

someFunc :: IO ()
someFunc = do
  print (isBigGang 4)
  print (isBigGang' 5)
  (_, y) <- execWriterT $ isBigGang'' 7
  putStrLn y
  (_, y) <- runWriterT $ isBigGang''' 4
  putStrLn y
  output <- runReaderT localExample "Lim"
  print output
