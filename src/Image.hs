{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Image where

import           Data.Massiv.Array
import           Data.Massiv.Array.IO      (Image)
import           Graphics.Pixel.ColorSpace

-- | l - linearity
-- | e - element
rgbToLab ::
     forall e. (Storable e, RealFloat e, Elevator e)
  => Image S (SRGB NonLinear) e
  -> Image S (LAB D65) e
rgbToLab array = compute $ mapStencil Edge filter array
  where
    filter =
      makeStencil
        (Sz 0)
        0
        (\get ->
           let (Pixel color) = get (0)
            in Pixel $ fromColorXYZ $ rgb2xyz color)

fromLists :: ColorModel cs e => [[Pixel cs e]] -> Image S cs e
fromLists = fromLists' Par
{-# INLINE fromLists #-}
