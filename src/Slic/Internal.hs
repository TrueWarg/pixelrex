{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Slic.Internal where

import           Data.Massiv.Array
import           Data.Massiv.Array         as A
import           Data.Massiv.Array.IO      (Image, convertImage)
import           Debug.Trace               (trace)
import           Graphics.Pixel.ColorSpace
import qualified Image                     as I

data Params =
  Params
    { superpixels :: Int
    , iterations  :: Int
    }

type Point2D = (Int, Int)

type PixelPoint cs e = (Pixel cs e, Point2D)

-- 1. Preprocessing
-- 2. Cluster centers
-- 3. Optimize the initial cluster center
-- 4. Calculate the distance between the pixel and the cluster center
-- 5. Classify pixels
-- 6. Recalculate cluster centers
-- 7. Interate 4 - 6
process ::
     (Integral e, Storable e, Elevator e)
  => Params
  -> Image S (SRGB 'NonLinear) e
  -> Image S (LAB D65) Float
process (Params superpixels iterations) image =
  process' iterations 0 initialClusters
  where
    mapped = (convertImage image) :: (Image D (LAB D65) Float)
    labImage = computeAs S $ mapped
    grads = sobelOperator $ computeAs S $ labImage
    Sz (w :. h) = size grads
    imagePixels = fromIntegral $ w * h
    length = floor $ sqrt $ imagePixels / (fromIntegral $ superpixels)
    initialClusters = initialCenters length labImage grads
    process' iterations step clusters =
      let mask = trace "!!! mask" assignClusters length labImage clusters
          newClusters = trace ("!!! newClusters" ++ show step) recalculateCenters labImage clusters mask
       in if (step == iterations) {- then error $ "!!! " ++ (show $ size initialClusters) ++ " " ++ show length ++ " " ++ show w  -}
            then makeArrayR
                   S
                   Par
                   (size mask)
                   (\(i :. j) ->
                      let (pixel, _) = mask !> i ! j
                       in pixel)
            else process' iterations (step + 1) newClusters

sobelOperator :: ColorModel cs e => Image S cs e -> Image S cs e
sobelOperator array =
  compute $ mapStencil Edge (makeCorrelationStencilFromKernel kernel) array
  where
    kernel = I.fromLists [[-1, 0, 1], [-2, 0, 2], [-1, 0, 1]]

initialCenters ::
     (ColorModel cs e, Ord (Color cs e))
  => Int
  -> Image S cs e
  -> Image S cs e
  -> Array U Ix1 (PixelPoint cs e)
initialCenters length original grads = centers
  where
    Sz (w :. h) = size grads
    gridw = (w `div` length) - 1
    gridh = (h `div` length) - 1
    idxElem i j =
      let i' = max i 0
          j' = max j 0
       in ((i', j'), original !> i' ! j', grads !> i' ! j')
    centers =
      makeArrayR
        U
        Par
        (Sz $ gridw * gridh)
        -- todo: make more general and efficient min index finding (try to find it in 'massive')
        (\k ->
           let i = k `div` (gridh - 1)
               j = k `mod` gridw
               cx = i * length
               cy = j * length
               flatWindow =
                 [ idxElem (cx - 1) (cy + 1)
                 , idxElem cx (cy + 1)
                 , idxElem (cx + 1) (cy + 1)
                 , idxElem (cx - 1) cy
                 , idxElem cx cy
                 , idxElem (cx + 1) cy
                 , idxElem (cx - 1) (cy - 1)
                 , idxElem cx (cy - 1)
                 , idxElem (cx + 1) (cy - 1)
                 ]
               localMinIdx =
                 indexOfMin $ fmap (\(_, _, grad) -> grad) flatWindow
               (coords, pixel, _) = flatWindow !! localMinIdx
            in (pixel, coords))

indexOfMin :: (Eq a, Ord a) => [a] -> Int
indexOfMin [] = error "List is empty"
indexOfMin (a:as) = indexOfMin' 0 a 1 as
  where
    indexOfMin' minIdx _ _ [] = minIdx
    indexOfMin' minIdx min idx (x:xs) =
      indexOfMin' newMinIdx newMin (idx + 1) xs
      where
        (newMinIdx, newMin) =
          if (x < min)
            then (idx, x)
            else (minIdx, min)

indexOfMinA :: (Eq a, Ord a, Storable a) => Array S Ix1 a -> Int
indexOfMinA arr
  | size arr == 0 = error "Array is empty"
  | otherwise = indexOfMinA' 0 (arr ! 0) 1 arr
  where
    indexOfMinA' minIdx min idx arr
      | size arr == (Sz idx) = minIdx
      | otherwise = indexOfMinA' newMinIdx newMin (idx + 1) arr
      where
        x = arr ! idx
        (newMinIdx, newMin) =
          if (x < min)
            then (idx, x)
            else (minIdx, min)

assignClusters ::
     (ColorModel cs e, Euclidean (Pixel cs e))
  => Int
  -> Image S cs e
  -> Array U Ix1 (PixelPoint cs e)
  -> Array U Ix2 (PixelPoint cs e)
assignClusters length image clusters = mask
  where
    neighborhood = fromIntegral $ length * length
    -- todo: now we have ~ O(w*h*k), where w and h - image size, k - cluster numbers.
    -- make more efficient impl with ~ O(k*2*S)
    -- (need PrimMonad for local mutablity or try to impl some inmutable version:
    -- store information between distance of clusters)
    mask =
      makeArrayR
        U
        Par
        (size image)
        (\(i :. j) ->
           let nearest =
                 computeAs U $
                 sfilter
                   (\(_, center) -> distance (i, j) center <= neighborhood)
                   clusters
               pixelPoint = (image !> i ! j, (i, j))
               distances =
                 computeAs S $
                 A.map
                   (\cluster -> slicDistance 0.5 pixelPoint cluster :: Float)
                   nearest
               index = indexOfMinA distances
               cluster = (computeAs U nearest) ! index
            in cluster)

recalculateCenters ::
     ColorModel cs e
  => Image S cs e
  -> Array U Ix1 (PixelPoint cs e)
  -> Array U Ix2 (PixelPoint cs e)
  -> Array U Ix1 (PixelPoint cs e)
recalculateCenters image old mask = new
  where
    newCoords cluster =
      let slice = sfilter (\pixelPoint -> pixelPoint == cluster) mask
          (x, y) =
            foldr
              (\(_, (i, j)) (accI, accJ) ->
                 (accI + fromIntegral i, accJ + fromIntegral j))
              (0, 0)
              slice
          (Sz sz) = size $ computeAs U $ slice
       in (floor $ x / fromIntegral sz, floor $ y / fromIntegral sz)
    new =
      computeAs U $
      smap
        (\cluster ->
           let (x, y) = newCoords cluster
            in (image !> x ! y, (x, y)))
        old

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
