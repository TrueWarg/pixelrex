{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Slic.Internal where

import           Data.Massiv.Array
import           Graphics.Pixel.ColorSpace
import qualified Image                     as I

data Params =
  Params

type Point2D = (Int, Int)

-- 1. Preprocessing
-- 2. Cluster centers
-- 3. Optimize the initial cluster center
-- 4. Calculate the distance between the pixel and the cluster center
-- 5. Classify pixels
-- 6. Recalculate cluster centers
-- 7. Interate 4 - 6
process :: (ColorModel cs e) => Params -> I.Image cs e -> I.Image cs e
process _ _ = error "Not implemented"

sobelOperator :: ColorModel cs e => I.Image cs e -> I.Image cs e
sobelOperator (I.Image array) =
  I.Image $
  compute $ mapStencil Edge (makeCorrelationStencilFromKernel kernel) array
  where
    (I.Image kernel) = I.fromLists [[-1, 0, 1], [-2, 0, 2], [-1, 0, 1]]

optimalCenters ::
     (ColorModel cs e, Ord (Color cs e))
  => Int
  -> I.Image cs e
  -> Array U Ix2 Point2D
optimalCenters superpixels (I.Image grads) = centers
  where
    Sz (w :. h) = size grads
    wStep = (w `div` superpixels) `div` 2 + 1
    hStep = (h `div` superpixels) `div` 2 + 1
    idxElem i j = ((i, j), grads !> i ! j)
    centers =
      makeArrayR
        U
        Par
        (Sz (superpixels - 1 :. superpixels - 1))
        -- todo: make more general and efficient min index finding (try to find it in 'massive')
        (\(i :. j) ->
           let cx = i * wStep
               cy = j * hStep
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
               localMinIdx = indexOfMin $ fmap (\(_, elem) -> elem) flatWindow
               ((minx, miny), _) = flatWindow !! localMinIdx
            in (minx, miny))

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
