module Pixelrex.Core.Algebra where

class VectorSpace v where
  (/+/) :: v -> v -> v
  (*/) :: Double -> v -> v

instance (VectorSpace a, VectorSpace b) => VectorSpace (a, b) where
  (x1, y1) /+/ (x2, y2) = (x1 /+/ x2, y1 /+/ y2)
  t */ (x, y) = (t */ x, t */ y)

instance (VectorSpace a, VectorSpace b, VectorSpace c) =>
         VectorSpace (a, b, c) where
  (x1, y1, z1) /+/ (x2, y2, z2) = (x1 /+/ x2, y1 /+/ y2, z1 /+/ z2)
  t */ (x, y, z) = (t */ x, t */ y, t */ z)

instance VectorSpace Double where
  a /+/ b = a + b
  a */ b = a * b
