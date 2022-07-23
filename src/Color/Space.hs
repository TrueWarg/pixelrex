{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Color.Space
  ( Pixel3D(..)
  , RGBPixel(..)
  , XYZPixel(..)
  , CIELABPixel(..)
  , coord
  , rgbToXyz
  , xyzToCIELAB
  , rgbToCIELAB
  ) where

import           Data.Massiv.Array

data Pixel3D a where
  Pixel3D :: Num a => a -> a -> a -> Pixel3D a

coord :: Int -> Pixel3D a -> a
coord 0 (Pixel3D a1 _ _) = a1
coord 1 (Pixel3D _ a2 _) = a2
coord 2 (Pixel3D _ _ a3) = a3
coord i _ = error $ "index " ++ show i ++ " must be in range [0, 2]"

type RGBPixel a = Pixel3D a

type XYZPixel a = Pixel3D a

type CIELABPixel a = Pixel3D a

-- See http://www.brucelindbloom.com/index.html?Eqn_RGB_to_XYZ.html
rgbToXyz :: (Floating a, Ord a) => RGBPixel a -> XYZPixel a
rgbToXyz pixel = result
  where
    (Pixel3D r g b) = energyPixel pixel
    x = r * 0.4124564 + g * 0.3575761 + b * 0.1804375
    y = r * 0.2126729 + g * 0.7151522 + b * 0.0721750
    z = r * 0.0193339 + g * 0.1191920 + b * 0.9503041
    result = Pixel3D x y z

energyPixel :: (Floating a, Ord a) => Pixel3D a -> Pixel3D a
energyPixel pixel =
  let normalize (Pixel3D r g b) = Pixel3D (r / 255) (g / 255) (b / 255)
      energyPoint p =
        if p <= 0.04045
          then p / 12.92
          else ((p + 0.055) / 1.055) ** 2.4
      energy (Pixel3D r g b) =
        Pixel3D (energyPoint r) (energyPoint g) (energyPoint b)
   in energy $ normalize pixel

-- See https://en.wikipedia.org/wiki/CIELAB_color_space
xyzToCIELAB :: (Floating a, Ord a) => XYZPixel a -> CIELABPixel a
xyzToCIELAB pixel = result
    -- https://en.wikipedia.org/wiki/Illuminant_D65
  where
    illuminantD65 (Pixel3D x y z) = Pixel3D (x / 0.950456) y (z / 1.088754)
    threshold value =
      if (value > epsilon)
        then value ** (1.0 / 3.0)
        else (kappa * value + 16.0) / 116.0
      where
        epsilon = 0.008856
        kappa = 903.3
    thresholdPixel (Pixel3D x y z) =
      Pixel3D (threshold x) (threshold y) (threshold z)
    (Pixel3D x y z) = thresholdPixel $ illuminantD65 pixel
    l = 116.0 * y - 16.0
    a = 500.0 * (x - y)
    b = 200.0 * (y - z)
    result = Pixel3D l a b

-- See http://www.brucelindbloom.com/index.html?Eqn_RGB_to_XYZ.html
-- See https://en.wikipedia.org/wiki/CIELAB_color_space
rgbToCIELAB :: (Prim a, Floating a, Ord a) => Array P (Ix3) a -> Array D (Ix3) a
rgbToCIELAB image = processed
  where
    processed =
      makeArrayR
        D
        Par
        (size image)
        -- todo: check if no redundant calculations (correct laziness work)
        (\(i :> j :. k) ->
           let r = image !> 0 !> j ! k
               g = image !> 1 !> j ! k
               b = image !> 2 !> j ! k
               pixel = xyzToCIELAB $ rgbToXyz $ Pixel3D r g b
            in coord i pixel)
