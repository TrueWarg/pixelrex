{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Pixelrex.Slic where

import           Control.Monad
import           Control.Monad.ST         (ST, runST)
import qualified Data.HashTable.Class     as H
import           Data.HashTable.ST.Linear (HashTable)
import           Data.Int                 (Int8)
import           Data.STRef
import           Debug.Trace              (trace)

import           Pixelrex.Array           (Ix2 (..), Matrix, Sz (..), loopM_,
                                           (!), (!>))

import           Data.Hashable
import qualified Pixelrex.Array           as A
import           Pixelrex.Distance
import           Pixelrex.Point

data Params =
  Params
    { superpixels    :: !Int
    , stride         :: !Int
    , iterations     :: !Int
    , distanceWeight :: !Float
    }

type Coord2D = Point2D Int

type SuperPixel a = (Point3D a, Coord2D)

type Image a = Matrix A.U (Point3D a)

type Clusters a = A.Array A.U A.Ix1 (SuperPixel a)

type ClustersMask a = A.Array A.U A.Ix2 (SuperPixel a)

instance Euclidean (Point2D Int) where
  distance (x1, y1) (x2, y2) = result
    where
      result = sqrt $ fromIntegral $ ((x2 - x1) ^ 2) + ((y2 - y1) ^ 2)

instance Euclidean (Point3D Float) where
  distance (x1, y1, z1) (x2, y2, z2) = result
    where
      result = sqrt $ ((x2 - x1) ^ 2) + ((y2 - y1) ^ 2) + ((z2 - z1) ^ 2)

process :: Params -> Image Float -> Image Float
process !(Params !superpixels !stride !iterations !weight) image =
  process' iterations 1 initialClusters
  where
    Sz (h :. w) = A.size image
    imagePixels = fromIntegral $ h * w
    length = floor $ sqrt $ imagePixels / (fromIntegral $ superpixels)
    initialClusters = initialCenters length image
    normalizedWeight = weight / fromIntegral length
    process' !iterations !step !clusters =
      let mask = assignClusters (3 * length) image clusters normalizedWeight
          newClusters = recalculateClusterCenters clusters mask
       in if (step == iterations)
            then let updatedMask = spreadPoolAndUpsample mask stride
                  in A.makeArrayR
                       A.U
                       A.Par
                       (A.size updatedMask)
                       (\(i :. j) ->
                          let (pixel, _) = updatedMask !> i ! j
                           in pixel)
            else process' iterations (step + 1) newClusters

spreadPoolAndUpsample ::
     (Hashable a, A.Unbox a, Show a) => ClustersMask a -> Int -> ClustersMask a
spreadPoolAndUpsample !mask !stride =
  runST $ do
    let maskSize@(Sz (maskH :. maskW)) = A.size mask
    newMask <- A.newMArray maskSize (mask !> 0 ! 0)
    loopM_ stride (< maskH) (+ stride) $ \i -> do
      loopM_ stride (< maskW) (+ stride) $ \j -> do
        let h = min stride (maskH - i)
            w = min stride (maskW - j)
            slice =
              A.computeAs A.U $
              A.flatten $ A.extract' (i :. j) (Sz $ h :. w) mask
            pixel = A.mostSpread slice
        A.writeToBlock_ newMask ((i, j), (i + h, j + w)) pixel
    A.freezeS newMask

localMinByGrad ::
     (A.Unbox a, Euclidean (Point3D a)) => Image a -> Coord2D -> Coord2D
localMinByGrad !image !center = runST $ localMinByGrad' image center
  where
    localMinByGrad' !image !(y, x) = do
      minRef <- newSTRef (fromIntegral (maxBound :: Int))
      pointRef <- newSTRef center
      let Sz (h :. w) = A.size image
          execute =
            loopM_ (y - 1) (\i -> i <= y + 1 && i >= 0 && i < h) (+ 1) $ \i -> do
              loopM_ (x - 1) (\j -> j <= x + 1 && j >= 0 && j < w) (+ 1) $ \j -> do
                let left = image !> i ! (max 0 (j - 1))
                    right = image !> i ! (min w (j + 1))
                    top = image !> (min h (i + 1)) ! j
                    bottom = image !> (max 0 (i - 1)) ! j
                    gx = distance left right
                    gy = distance top bottom
                    gradient = (gx ^ 2) + (gy ^ 2)
                min <- readSTRef minRef
                if (gradient < min)
                  then writeSTRef minRef min *> writeSTRef pointRef (i, j)
                  else pure ()
      -- execute
      readSTRef pointRef

initialCenters ::
     (A.Unbox a, Euclidean (Point3D a)) => Int -> Image a -> Clusters a
initialCenters !step !image = centers
  where
    Sz (h :. w) = A.size image
    gridh = (h `div` step) - 1
    gridw = (w `div` step) - 1
    centers =
      A.makeArrayR
        A.U
        A.Par
        (Sz $ gridh * gridw)
        (\k ->
           let i = k `div` (gridh - 1)
               j = k `mod` gridw
               cy = min (h - 1) (i * step)
               cx = min (w - 1) (j * step)
               coords@(cy', cx') = localMinByGrad image (cy, cx)
            in (image !> cy' ! cx', coords))

assignClusters ::
     (A.Unbox a, Euclidean (Point3D a))
  => Int
  -> Image a
  -> Clusters a
  -> Float
  -> ClustersMask a
assignClusters !neighborhood !image !clusters !distWeight = runST $ result
  where
    imageSize@(Sz (h :. w)) = A.size image
    result = do
      mask <- A.newMArray imageSize (clusters ! 0)
      distances <-
        A.newMArray imageSize (fromIntegral (maxBound :: Int)) :: ST s (A.MArray (A.PrimState (ST s)) A.S Ix2 Float)
      let Sz clusterSize = A.size clusters
          execute =
            loopM_ 0 (< clusterSize) (+ 1) $ \k -> do
              let cluster@(clusterPixel, (y, x)) = clusters ! k
                  startY = max 0 (y - neighborhood)
                  endY = min h (y + neighborhood)
                  startX = max 0 (x - neighborhood)
                  endX = min w (x + neighborhood)
              loopM_ startY (< endY) (+ 1) $ \i -> do
                loopM_ startX (< endX) (+ 1) $ \j -> do
                  let imagePixel = image !> i ! j
                      distance =
                        slicDistance distWeight cluster (imagePixel, (i, j))
                  currentDistance <- A.readM distances (i :. j)
                  if (distance < currentDistance)
                    then A.write_ distances (i :. j) distance *>
                         A.write_ mask (i :. j) cluster
                    else pure ()
      execute
      A.freezeS mask

recalculateClusterCenters ::
     (A.Unbox a, Eq a, RealFrac a, Hashable a)
  => Clusters a
  -> ClustersMask a
  -> Clusters a
recalculateClusterCenters !old !mask = runST result
  where
    Sz (clustersCount) = A.size old
    Sz (h :. w) = A.size mask
    deriveElemenwise (!a1, !a2, !a3) !v = (a1 / v, a2 / v, a3 / v)
    result = do
      let calculateCoordSums clustersCount = do
            coordSums <- H.newSized clustersCount :: ST s (HashTable s k v)
            loopM_ 0 (< h) (+ 1) $ \i -> do
              loopM_ 0 (< w) (+ 1) $ \j -> do
                let superpixel@(pixel, coord) = mask !> i ! j
                item <- H.lookup coordSums coord
                case item of
                  Just (!accPixel, !accY, !accX, !count) ->
                    H.insert
                      coordSums
                      coord
                      (accPixel + pixel, accY + i, accX + j, count + 1)
                  Nothing -> H.insert coordSums coord (pixel, i, j, 1)
            pure coordSums
          calculateClusters clustersCount sums = do
            clusters <- A.newMArray (Sz clustersCount) ((0, 0, 0), (0, 0))
            clusterIdxRef <- newSTRef 0
            -- todo: it's better to use something like loop with indexing. Try impl
            H.mapM_
              (\(_, (!accPixel, !accY, !accX, !count)) -> do
                 idx <- readSTRef clusterIdxRef
                 let n = fromIntegral count
                     fracAccY = fromIntegral accY
                     fracAccX = fromIntegral accX
                     pixel = deriveElemenwise accPixel n
                     coord@(y, x) = (floor $ fracAccY / n, floor $ fracAccX / n)
                  in A.write_ clusters idx (pixel, coord) *>
                     writeSTRef clusterIdxRef (idx + 1))
              sums
            pure clusters
      sums <- calculateCoordSums clustersCount
      new <- calculateClusters clustersCount sums
      A.freezeS new

slicDistance ::
     Euclidean (Point3D a) => Float -> SuperPixel a -> SuperPixel a -> Float
slicDistance weight (pixel1, coord1) (pixel2, coord2) =
  colorDistance + weight * spaceDistance
  where
    colorDistance = distance pixel1 pixel2
    spaceDistance = distance coord1 coord2
