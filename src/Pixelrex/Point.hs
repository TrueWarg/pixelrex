{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Pixelrex.Point
  ( Point3D
  , Point2D
  ) where

import           Pixelrex.Distance

type Point3D a = (a, a, a)

type Point2D a = (a, a)

-- todo: impl other methods or linear space class
instance (Num a) => Num (Point3D a) where
  (a1, b1, c1) + (a2, b2, c2) = (a1 + a2, b1 + b2, c1 + c2)

