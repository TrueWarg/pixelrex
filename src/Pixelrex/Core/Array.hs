{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-------------------------------------------------------------------------------------------
module Pixelrex.Core.Array
  ( module Data.Massiv.Array
  , module Data.Massiv.Array.IO
  , Point2D
  , writeToBlock_
  , mostSpread
  , submatrix
  , frameOfMutable
  , submatrixOfMutable
  , maxBy
  , indexOfMaxBy
  , pickValues
  , patch
  , patchOfMutable
  ) where

import           Control.Monad.ST        (ST, runST)
import           Data.Hashable
import           Data.HashTable.Class    as H
import           Data.HashTable.ST.Basic as BH
import           Data.Massiv.Array
import           Data.Massiv.Array       as A
import           Data.Massiv.Array.IO    hiding (Image)
import           Data.Maybe              (fromJust)
import           Data.STRef
import           Pixelrex.Core.Algebra   (circle0)
import           Pixelrex.Core.Point     (Point2D)

-------------------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------------------
frameOfMutable ::
     forall m r a. (PrimMonad m, Manifest r a)
  => MMatrix (PrimState m) r a
  -> Ix2
  -> Ix2
  -> m (Vector r a)
frameOfMutable matrix (y1' :. x1') (y2' :. x2') = do
  collected <- frameOfMutable' (y1 + 1) x1 A.empty
  return $ A.computeAs (undefined :: r) collected
  where
    y1 = max y1' 0
    x1 = max x1' 0
    y2 = min y2' (h - 1)
    x2 = min x2' (w - 1)
    Sz (h :. w) = A.sizeOfMArray matrix
    frameOfMutable' i j frame
      | j < x1 = return frame
      | otherwise = do
        value <- A.read matrix (i :. j)
        let
          (nextI, nextJ) = case (i == y2, j == x2) of
            (False, False) -> if (i == y1) then (i, j - 1) else (i + 1, j)
            (True, False)  -> (i, j + 1)
            (True, True)   -> (i - 1, j)
            (False, True)  -> if (i == y1) then (i, j - 1) else  (i - 1, j)
        case value of
          Just v  -> frameOfMutable' nextI nextJ (v `cons` frame)
          Nothing -> frameOfMutable' nextI nextJ frame

-------------------------------------------------------------------------------------------
submatrixOfMutable ::
     forall m r a. (PrimMonad m, MonadThrow m, Manifest r a)
  => MMatrix (PrimState m) r a
  -> Ix2
  -> Ix2
  -> m (Matrix r a)
submatrixOfMutable matrix (y1 :. x1) (y2 :. x2) =
  A.makeArrayA size $ \(i :. j) -> A.readM matrix (startY + i :. startX + j)
  where
    Sz (h :. w) = A.sizeOfMArray matrix
    startY = max y1 0
    startX = max x1 0
    size = Sz $ ((min (y2 + 1) h) - startY) :. ((min (x2 + 1) w) - startX)

-------------------------------------------------------------------------------------------
submatrix ::
     forall r a. (Manifest r a, Load r Ix2 a)
  => Matrix r a
  -> Ix2
  -> Ix2
  -> Matrix r a
submatrix matrix (y1 :. x1) (y2 :. x2) =
  A.makeArray A.Par size $ \(i :. j) -> matrix !> (startY + i) ! (startX + j)
  where
    Sz (h :. w) = A.size matrix
    startY = max y1 0
    startX = max x1 0
    size = Sz $ ((min (y2 + 1) h) - startY) :. ((min (x2 + 1) w) - startX)

-------------------------------------------------------------------------------------------
maxBy ::
     forall r a b. (Manifest r a, Ord b)
  => (a -> b)
  -> Vector r a
  -> a
maxBy criterion vector = vector ! idx
  where
    idx = indexOfMaxBy criterion vector

-------------------------------------------------------------------------------------------
indexOfMaxBy ::
     forall r a b. (Manifest r a, Ord b)
  => (a -> b)
  -> Vector r a
  -> Int
indexOfMaxBy criterion vector
  | A.isEmpty vector = error "vector is empty"
  | otherwise = indexOfMaxBy' (vector ! 0) 0 1
  where
    Sz size = A.size vector
    indexOfMaxBy' max idxMax idx
      | idx == size = idxMax
      | otherwise =
        let value = vector ! idx
            (max', idxMax') =
              if criterion value > criterion max
                then (value, idx)
                else (max, idxMax)
         in indexOfMaxBy' max' idxMax' (idx + 1)

-------------------------------------------------------------------------------------------
pickValues ::
     forall r a. (Manifest r a)
  => Matrix r a
  -> Matrix A.U Ix2
  -> Matrix A.B (Maybe a)
pickValues matrix indices =
  A.makeArray A.Par (A.size indices) $ \(i :. j) ->
    let idx = indices !> i ! j
     in A.index matrix idx

-------------------------------------------------------------------------------------------
patchOfMutable ::
     forall m r a. (PrimMonad m, MonadThrow m, Manifest r a)
  => MMatrix (PrimState m) r a
  -> Ix2
  -> Ix2
  -> m (Matrix r a)
patchOfMutable matrix (y1 :. x1) (y2 :. x2) =
  A.makeArrayA size $ \(i :. j) -> do
    let i' = circle0 i h
        j' = circle0 j w
    A.readM matrix (i' :. j')
  where
    Sz (h :. w) = A.sizeOfMArray matrix
    size = Sz $ (y2 - y1) :. (x2 - x1)

-------------------------------------------------------------------------------------------
patch ::
     forall r a. (Manifest r a, Load r Ix2 a)
  => Matrix r a
  -> Ix2
  -> Ix2
  -> Matrix r a
patch matrix (y1 :. x1) (y2 :. x2) =
  A.makeArray A.Par size $ \(i :. j) ->
    let i' = circle0 i h
        j' = circle0 j w
     in matrix !> i' ! j'
  where
    Sz (h :. w) = A.size matrix
    size = Sz $ (y2 - y1) :. (x2 - x1)
