module Pixelrex.Core.Debug where

import           Debug.Trace         (trace)
import           Pixelrex.Core.Array (Vector, (!))
import qualified Pixelrex.Core.Array as A

logVector :: (A.Manifest r a, Show a) => Vector r a -> Vector r a
logVector vec = logVector' 0 vec
  where
    A.Sz size = A.size vec
    logVector' idx vector
      | idx == size = vector
      | otherwise = trace (show $ vector ! idx) (logVector' (idx + 1) vector)
{-# INLINE logVector #-}

echo :: (Show a) => a -> a
echo a = trace (show a) a
{-# INLINE echo #-}