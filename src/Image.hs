{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Image where

import           Data.Massiv.Array
import           Graphics.Pixel.ColorSpace

-- | cs - color space
-- | e - element
data Image cs e =
  Image
    { array :: !(Array S Ix2 (Pixel cs e))
    }

-- | l - linearity
-- | e - element
rgbToLab ::
     forall e. (Storable e, RealFloat e, Elevator e)
  => Image (SRGB NonLinear) e
  -> Image (LAB D65) e
rgbToLab (Image array) = Image $ compute $ mapStencil Edge filter array
  where
    filter =
      makeStencil
        (Sz 0)
        0
        (\get ->
           let (Pixel color) = get (0)
            in Pixel $ fromColorXYZ $ rgb2xyz color)

fromLists :: ColorModel cs e => [[Pixel cs e]] -> Image cs e
fromLists = Image . fromLists' Par
{-# INLINE fromLists #-}
