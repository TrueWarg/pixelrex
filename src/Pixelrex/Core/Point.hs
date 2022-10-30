{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

-------------------------------------------------------------------------------------------
module Pixelrex.Core.Point
  ( Point3D
  , Point2D
  ) where

import           Pixelrex.Core.Algebra

-------------------------------------------------------------------------------------------
type Point3D a = (a, a, a)

type Point2D a = (a, a)

-------------------------------------------------------------------------------------------
-- todo: impl linear space (VectorSpace) class instead
instance (Num a) => Num (Point3D a) where
  (!a1, !b1, !c1) + (!a2, !b2, !c2) = (a1 + a2, b1 + b2, c1 + c2)
