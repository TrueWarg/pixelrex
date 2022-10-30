-- Module with function for implimentation of SLIC (Simple Linear Iterative Clustering) algorithm.
-- See details in https://www.iro.umontreal.ca/~mignotte/IFT6150/Articles/SLIC_Superpixels.pdf
-- Radhakrishna Achanta, Appu Shaji, Kevin Smith, Aurelien Lucchi,
-- Pascal Fua, and Sabine Susstrunk, SLIC Superpixels, EPFL Technical Report no. 149300, June 2010.
{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

-------------------------------------------------------------------------------------------
module Pixelrex.Segmentation.Slic where

import           Control.Monad
import           Control.Monad.ST         (ST, runST)
import qualified Data.HashTable.Class     as H
import           Data.HashTable.ST.Linear (HashTable)
import           Data.Int                 (Int8)
import           Data.STRef
import           Debug.Trace              (trace)

import           Pixelrex.Core.Array      (Ix2 (..), Matrix, Sz (..), loopM_,
                                           (!), (!>))

import           Data.Hashable
import qualified Pixelrex.Core.Array      as A
import           Pixelrex.Core.Metric     (L2(..))
import           Pixelrex.Core.Point

-------------------------------------------------------------------------------------------
data Params =
  Params
    { clusterLength       :: !Int -- ^ length of each cluster, in other words count of pixel of square side
    , stride              :: !Int -- ^ count of pixel/square side, which will be merged in square with same color (final pixelization)
    , iterations          :: !Int -- ^ number of applications main steps
    , spaceDistanceWeight :: !Float -- value in SLIC distance formula: SLICdistance = ColorDistance + (w / length) * SpaceDistance
    }

type Coord2D = Point2D Int

type SuperPixel a = (Point3D a, Coord2D)

type Image a = Matrix A.U (Point3D a)

type Clusters a = A.Array A.U A.Ix1 (SuperPixel a)

type ClustersMask a = A.Array A.U A.Ix2 (SuperPixel a)

-------------------------------------------------------------------------------------------
instance L2 (Point2D Int) where
  l2Distance (x1, y1) (x2, y2) = result
    where
      result = sqrt $ fromIntegral $ ((x2 - x1) ^ 2) + ((y2 - y1) ^ 2)

instance L2 (Point3D Float) where
  l2Distance (x1, y1, z1) (x2, y2, z2) = result
    where
      result = sqrt $ ((x2 - x1) ^ 2) + ((y2 - y1) ^ 2) + ((z2 - z1) ^ 2)

-------------------------------------------------------------------------------------------
-- | Process main SLIC with defined params
processSlic :: Params -> Image Float -> Image Float
processSlic !(Params !length !stride !iterations !weight) image =
  process' iterations 1 initialClusters
  where
    Sz (h :. w) = A.size image
    initialClusters = initialCenters length image
    normalizedWeight = weight / fromIntegral length
    process' !iterations !step !clusters =
      let mask = assignClusters (3 * length) image clusters normalizedWeight
          newClusters = recalculateClusterCenters clusters mask
       in if (step == iterations)
            then let updatedMask = keepSpreadIn mask stride
                  in A.makeArrayR
                       A.U
                       A.Par
                       (A.size updatedMask)
                       (\(i :. j) ->
                          let (pixel, _) = updatedMask !> i ! j
                           in pixel)
            else process' iterations (step + 1) newClusters

-------------------------------------------------------------------------------------------
-- | Take most spread pixel in square and replace with it other pixels in this square
-- | 0 1 | 0 0 |        | 2 2 | 0 0 |
-- | 2 2 | 0 1 |        | 2 2 | 0 0 |
-- |-----|-----|  ----> |-----|-----|
-- | 3 3 | 3 3 |        | 3 3 | 3 3 |
-- | 1 2 | 2 1 |        | 3 3 | 3 3 |
keepSpreadIn ::
     (Hashable a, A.Unbox a, Show a) => ClustersMask a -> Int -> ClustersMask a
keepSpreadIn !mask !stride =
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

-------------------------------------------------------------------------------------------
-- | Find coordinate of min image gradient in square 3x3 with defined center
coordinateOfMinGrad ::
     (A.Unbox a, L2 (Point3D a)) => Image a -> Coord2D -> Coord2D
coordinateOfMinGrad !image !center = runST $ localMinByGrad' image center
  where
    localMinByGrad' !image !(y, x) = do
      minRef <- newSTRef (fromIntegral (maxBound :: Int))
      pointRef <- newSTRef center
      let Sz (h :. w) = A.size image
          execute =
            loopM_ (y - 1) (\i -> i <= y + 1 && i >= 0 && i < h) (+ 1) $ \i -> do
              loopM_ (x - 1) (\j -> j <= x + 1 && j >= 0 && j < w) (+ 1) $ \j -> do
                let left = image !> i ! (max 0 (j - 1))
                    right = image !> i ! (min (w - 1) (j + 1))
                    top = image !> (min (h - 1) (i + 1)) ! j
                    bottom = image !> (max 0 (i - 1)) ! j
                    gx = l2Distance left right
                    gy = l2Distance top bottom
                    gradient = (gx ^ 2) + (gy ^ 2)
                min <- readSTRef minRef
                if (gradient < min)
                  then writeSTRef minRef min *> writeSTRef pointRef (i, j)
                  else pure ()
      execute
      readSTRef pointRef

-------------------------------------------------------------------------------------------
-- | Generate initial centers, which will be represented
-- by a non-uniform grid corrected with 'coordinateOfMinGrad'
initialCenters :: (A.Unbox a, L2 (Point3D a)) => Int -> Image a -> Clusters a
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
               coords@(cy', cx') = coordinateOfMinGrad image (cy, cx)
            in (image !> cy' ! cx', coords))

-------------------------------------------------------------------------------------------
-- | In defined neighborhood for each pixel find slic disntance beetween the nearest
-- by coordinate clusters and assign cluster with minimu distance for this pixel
assignClusters ::
     (A.Unbox a, L2 (Point3D a))
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

-------------------------------------------------------------------------------------------
-- | For each cluster in superpixels mask find average of cordinates and colors,
--   next define new cluster with these averages.
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

-------------------------------------------------------------------------------------------
-- | SLICDistance = ColorDistance + Weight * SpaceDistance
slicDistance :: L2 (Point3D a) => Float -> SuperPixel a -> SuperPixel a -> Float
slicDistance weight (pixel1, coord1) (pixel2, coord2) =
  colorDistance + weight * spaceDistance
  where
    colorDistance = l2Distance pixel1 pixel2
    spaceDistance = l2Distance coord1 coord2
