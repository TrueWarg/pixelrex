{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Slic.Internal where

import           Control.Monad
import           Control.Monad.ST          (ST, runST)
import           Data.Int                  (Int8)
import           Data.Massiv.Array
import           Data.Massiv.Array         as A
import           Data.Massiv.Array.IO      (Image, convertImage)
import           Data.STRef
import           Debug.Trace               (trace)
import           Graphics.Pixel.ColorSpace
import qualified Image                     as I

data Params =
  Params
    { superpixels    :: !Int
    , stride         :: !Int
    , iterations     :: !Int
    , distanceWeight :: !Float
    }

type Point2D = (Int, Int)

type PixelPoint cs e = (Pixel cs e, Point2D)

process ::
     (Integral e, Storable e, Elevator e)
  => Params
  -> Image S (SRGB 'NonLinear) e
  -> Image S (LAB D65) Float
process (Params superpixels stride iterations weight) image =
  process' iterations 1 initialClusters
  where
    mapped = (convertImage image) :: (Image D (LAB D65) Float)
    labImage = computeAs S $ mapped
    Sz (h :. w) = size image
    imagePixels = fromIntegral $ h * w
    length = floor $ sqrt $ imagePixels / (fromIntegral $ superpixels)
    initialClusters = initialCenters length labImage
    process' iterations step clusters =
      let mask =
            assignClusters
              (3 * length)
              labImage
              clusters
              (slicDistance (weight / fromIntegral length))
          newClusters = recalculateCenters labImage clusters mask
       in if (step == iterations)
            then makeArrayR
                   S
                   Par
                   (size mask)
                   (\(i :. j) ->
                      let (pixel, _) = mask !> i ! j
                       in pixel)
            else process' iterations (step + 1) clusters


sobelOperator :: ColorModel cs e => Image S cs e -> Image S cs e
sobelOperator array =
  compute $ mapStencil Edge (makeCorrelationStencilFromKernel kernel) array
  where
    kernel = I.fromLists [[-1, 0, 1], [-2, 0, 2], [-1, 0, 1]]

localMinByGrad ::
     (ColorModel cs e, Euclidean (Pixel cs e))
  => Image S cs e
  -> Point2D
  -> Point2D
localMinByGrad image center = runST $ localMinByGrad' image center
  where
    localMinByGrad' !image !(y, x) = do
      minRef <- newSTRef (fromIntegral (maxBound :: Int))
      pointRef <- newSTRef center
      let Sz (h :. w) = size image
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
      readSTRef pointRef

initialCenters ::
     (ColorModel cs e, Euclidean (Pixel cs e))
  => Int
  -> Image S cs e
  -> Array U Ix1 (PixelPoint cs e)
initialCenters step image = centers
  where
    Sz (h :. w) = size image
    gridh = (h `div` step) - 1
    gridw = (w `div` step) - 1
    centers =
      makeArrayR
        U
        Par
        (Sz $ gridh * gridw)
        (\k ->
           let i = k `div` (gridh - 1)
               j = k `mod` gridw
               cy = min (h - 1) (i * step)
               cx = min (w - 1) (j * step)
               coords@(cy', cx') = localMinByGrad image (cy, cx)
            in (image !> cy' ! cx', coords))

assignClusters ::
     (ColorModel cs e, Euclidean (Pixel cs e))
  => Int
  -> Image S cs e
  -> Array U Ix1 (PixelPoint cs e)
  -> (PixelPoint cs e -> PixelPoint cs e -> Float)
  -> Array U Ix2 (PixelPoint cs e)
assignClusters neighborhood image clusters distanceFunc = runST $ result
  where
    imageSize@(Sz (h :. w)) = size image
    result = do
      mask <- newMArray imageSize (clusters ! 5)
      distances <-
        newMArray imageSize (fromIntegral (maxBound :: Int)) :: ST s (A.MArray (A.PrimState (ST s)) S Ix2 Float)
      let Sz clusterSize = size clusters
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
                      distance = distanceFunc cluster (imagePixel, (i, j))
                  currentDistance <- A.readM distances (i :. j)
                  if (distance < currentDistance)
                    then write_ distances (i :. j) distance *>
                         write_ mask (i :. j) cluster
                    else pure ()
      execute
      freezeS mask

recalculateCenters ::
     (ColorModel cs e, Fractional e)
  => Image S cs e
  -> Array U Ix1 (PixelPoint cs e)
  -> Array U Ix2 (PixelPoint cs e)
  -> Array U Ix1 (PixelPoint cs e)
recalculateCenters image old mask = new
  where
    newCoords cluster =
      let (initialPixel, _) = cluster
          slice = sfilter (\pixelPoint -> pixelPoint == cluster) mask
          (pixel, y, x) =
            foldr
              (\(pixel, (i, j)) (accPixel, accI, accJ) ->
                 ( accPixel + pixel
                 , accI + fromIntegral i
                 , accJ + fromIntegral j))
              (initialPixel, 0, 0)
              slice
          (Sz sz) = size $ computeAs U $ slice
       in ( pixel / fromIntegral sz
          , (floor $ y / fromIntegral sz, floor $ x / fromIntegral sz))
    new = computeAs U $ smap newCoords old

slicDistance ::
     (ColorModel cs e, Euclidean (Pixel cs e))
  => Float
  -> PixelPoint cs e
  -> PixelPoint cs e
  -> Float
slicDistance weight (pixel1, coord1) (pixel2, coord2) =
  colorDistance + weight * spaceDistance
  where
    colorDistance = distance pixel1 pixel2
    spaceDistance = distance coord1 coord2

class Euclidean a where
  distance :: a -> a -> Float

instance Euclidean Point2D where
  distance (x1, y1) (x2, y2) = result
    where
      result = sqrt $ fromIntegral $ ((x2 - x1) ^ 2) + ((y2 - y1) ^ 2)

instance Euclidean (Pixel (LAB i) Float) where
  distance (Pixel (ColorLAB l1 a1 b1)) (Pixel (ColorLAB l2 a2 b2)) = result
    where
      result = sqrt $ ((b2 - b1) ^ 2) + ((a2 - a1) ^ 2) + ((l2 - l1) ^ 2)
