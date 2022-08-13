{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Pixelrex.Point
  ( Point3D
  , Point2D
  ) where

import           Pixelrex.Distance

type Point3D a = (a, a, a)

type Point2D a = (a, a)

-- todo: impl other methods
instance (Num a) => Num (Point3D a) where
  (a1, b1, c1) + (a2, b2, c2) = (a1 + a2, b1 + b2, c1 + c2)

-- Num (Point3D a), Fractional (Point3D a)


-- instance (Floating a) => Euclidean (Point3D a) where
--   distance (x1, y1, z1) (x2, y2, z2) = result
--     where
--       result =
--         fromFloating $ sqrt $ ((x2 - x1) ^ 2) + ((y2 - y1) ^ 2) + ((z2 - z1) ^ 2)
