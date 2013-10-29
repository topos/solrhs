module TimeIt (time) where

import Data.Time

time :: IO a -> IO (a,Double)
time io = do
  t0 <- getCurrentTime
  a <- io
  t1 <- getCurrentTime
  return (a,realToFrac (t1 `diffUTCTime` t0))