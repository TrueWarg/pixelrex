{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pixelrex.Array
  ( module Data.Massiv.Array
  , module Data.Massiv.Array.IO
  , Point2D
  , writeToBlock_
  , mostSpread
  ) where

import           Control.Monad.ST        (ST, runST)
import           Data.HashTable.Class    as H
import           Data.HashTable.ST.Basic as BH
import           Data.Hashable
import           Data.Massiv.Array
import           Data.Massiv.Array       as A
import           Data.Massiv.Array.IO    hiding (Image)
import           Data.STRef
import           Pixelrex.Point          (Point2D)

writeToBlock_ ::
     (Manifest r e, PrimMonad m)
  => MMatrix (PrimState m) r e
  -> (Point2D Int, Point2D Int)
  -> e
  -> m ()
writeToBlock_ !arr !(topLeft, bottonRight) value = do
  let (Sz (h :. w)) = sizeOfMArray arr
      (startY, startX) = topLeft
      (endY, endX) = bottonRight
      (safeStartY, safeStartX) = (max 0 startY, max 0 startX)
      (safeEndY, safeEndX) = (min h endY, min w endX)
  loopM_ safeStartY (< safeEndY) (+ 1) $ \i -> do
    loopM_ safeStartX (< safeEndX) (+ 1) $ \j -> do write_ arr (i :. j) value

mostSpread :: (Hashable e, Eq e, Manifest r e) => Array r Ix1 e -> e
mostSpread arr =
  runST $ do
    let Sz arrSize = A.size arr
    counts <- H.new :: ST s (BH.HashTable s k v)
    resultRef <- newSTRef (arr ! 0)
    maxRef <- newSTRef 1
    loopM_ 0 (< arrSize) (+ 1) $ \i -> do
      let item = arr ! i
      value <- H.lookup counts item
      case value of
        Just count -> do
          H.insert counts item (count + 1)
          max <- readSTRef maxRef
          if (count + 1 > max)
            then writeSTRef maxRef (count + 1) *> writeSTRef resultRef item
            else pure ()
        Nothing -> H.insert counts item 1
    readSTRef resultRef
