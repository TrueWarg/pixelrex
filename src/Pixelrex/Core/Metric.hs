module Pixelrex.Core.Metric
  ( L2(..)
  , L1(..)
  ) where

class L2 a where
  l2Distance :: a -> a -> Float

class L1 a where
  l1Distance :: a -> a -> Float
