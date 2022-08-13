{-# LANGUAGE BangPatterns #-}

module Pixelrex.Array
  ( module Data.Massiv.Array
  , module Data.Massiv.Array.IO
  , Point2D
  , writeToBlock_
  ) where

import           Data.Massiv.Array
import           Data.Massiv.Array.IO hiding (Image)
import           Pixelrex.Point       (Point2D)

writeToBlock_ ::
     (Manifest r e, PrimMonad m)
  => MMatrix (PrimState m) r e
  -> (Point2D Int, Point2D Int)
  -> e
  -> m ()
writeToBlock_ !arr !(topLeft, bottonRight) value = do
  let (Sz (h :. w)) = sizeOfMArray arr
      (startY, startX) = topLeft
      (endY, endX) = topLeft
      (safeStartY, safeStartX) = (max 0 startY, max 0 startX)
      (safeEndY, safeEndX) = (min h endY, min w endX)
  loopM_ safeStartY (< safeEndY) (+ 1) $ \i -> do
    loopM_ safeStartX (< safeEndX) (+ 1) $ \j -> do write_ arr (i :. j) value
