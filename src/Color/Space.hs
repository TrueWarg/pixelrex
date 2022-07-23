{-# LANGUAGE GADTs #-}

module Color.Space where

data Pixel3D a where
  Pixel3D :: Num a => a -> a -> a -> Pixel3D a

newtype RGBPixel a =
  RGBPixel (Pixel3D a)

newtype XYZPixel a =
  XYZPixel (Pixel3D a)

newtype CIELABPixel a =
  CIELABPixel (Pixel3D a)

-- See http://www.brucelindbloom.com/index.html?Eqn_RGB_to_XYZ.html
rgbToXyz :: (Floating a, Ord a) => RGBPixel a -> XYZPixel a
rgbToXyz (RGBPixel pixel) = result
  where
    normalize (Pixel3D r g b) = Pixel3D (r / 255) (g / 255) (b / 255)
    energyPoint p =
      if p <= 0.04045
        then p / 12.92
        else ((p + 0.055) / 1.055) ** 2.4
    energy (Pixel3D r g b) =
      Pixel3D (energyPoint r) (energyPoint g) (energyPoint b)
    (Pixel3D r g b) = energy $ normalize pixel
    x = r * 0.4124564 + g * 0.3575761 + b * 0.1804375
    y = r * 0.2126729 + g * 0.7151522 + b * 0.0721750
    z = r * 0.0193339 + g * 0.1191920 + b * 0.9503041
    result = XYZPixel $ Pixel3D x y z

-- See https://en.wikipedia.org/wiki/CIELAB_color_space
xyzToCIELABP :: (Floating a, Ord a) => XYZPixel a -> CIELABPixel a
xyzToCIELABP (XYZPixel pixel) = result
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
    result = CIELABPixel $ Pixel3D l a b
