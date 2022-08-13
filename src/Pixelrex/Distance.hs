module Pixelrex.Distance
  ( Euclidean(..)
  ) where

class Euclidean a where
  distance :: a -> a -> Float
