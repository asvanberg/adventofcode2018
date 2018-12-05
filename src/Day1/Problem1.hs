{-# LANGUAGE FlexibleContexts #-}
module Day1.Problem1 where

import Control.Monad.IO.Class
import Control.Monad.State
import Data.List (isPrefixOf)
import System.IO hiding (isEOF)

problem1 :: IO ()
problem1 = do
  h <- openFile "src/Day1/input" ReadMode
  f <- execStateT (frequency h) 0
  hClose h
  print f

frequency :: (MonadIO m, MonadState Int m) => Handle -> m ()
frequency handle =
  flip unless loop =<< isEOF handle
  where
    loop = readNumber handle >>= modify . (+) >> frequency handle

frequency2 :: (MonadIO m, MonadState Int m) => Handle -> m ()
frequency2 handle =
  isEOF handle >>= iif
    (return ())
    (readNumber handle >>= modify . (+) >> frequency2 handle)

frequencyDo :: (MonadIO m, MonadState Int m) => Handle -> m ()
frequencyDo handle = do
  eof <- isEOF handle
  if eof
    then return ()
    else do
      number <- readNumber handle
      modify (+ number)
      frequencyDo handle

isEOF :: (MonadIO m) => Handle -> m Bool
isEOF = liftIO . hIsEOF

readNumber :: (MonadIO m) => Handle -> m Int
readNumber h =
  liftIO $ read . clean <$> hGetLine h
  where
    clean = iif tail id =<< isPrefixOf "+"

iif :: a -> a -> Bool -> a
iif t f b = if b then t else f
