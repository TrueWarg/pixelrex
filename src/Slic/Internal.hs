{-# LANGUAGE DataKinds #-}

module Slic.Internal where

import           Data.Massiv.Array
import           Graphics.Pixel.ColorSpace
import qualified Image                     as I

data Params =
  Params

-- 1. Preprocessing
-- 2. Cluster centers
-- 3. Optimize the initial cluster center
-- 4. Calculate the distance between the pixel and the cluster center
-- 5. Classify pixels
-- 6. Recalculate cluster centers
-- 7. Interate 4 - 6
process :: (Prim a, Num a) => Params -> Array P (Ix3) a -> Array D (Ix3) a
process _ _ = error "Not implemented"

clusterCenters :: (Prim a, Num a) => Int -> Array P (Ix3) a -> Array D (Ix3) a
clusterCenters superpixels image = centers
  where
    Sz (ch :> w :. h) = size image
    wStep = (w `div` superpixels) `div` 2
    hStep = (h `div` superpixels) `div` 2
    centers =
      makeArrayR
        D
        Par
        (Sz (ch :> superpixels :. superpixels))
        (\(i :> j :. k) -> image !> i !> (j * wStep) ! (k * hStep))

sobelOperator :: ColorModel cs e => I.Image cs e -> I.Image cs e
sobelOperator (I.Image array) =
  I.Image $
  compute $ mapStencil Edge (makeCorrelationStencilFromKernel kernel) array
  where
    (I.Image kernel) = I.fromLists [[-1, 0, 1], [-2, 0, 2], [-1, 0, 1]]
