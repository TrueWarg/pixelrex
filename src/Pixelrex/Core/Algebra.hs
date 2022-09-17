module Pixelrex.Core.Algebra where

class VectorSpace v where
  {-# MINIMAL (/+/), (*/) #-}
  (/+/) :: v -> v -> v
  (/-/) :: v -> v -> v
  v1 /-/ v2 = v1 /+/ negativeV v2
  (*/) :: Double -> v -> v
  negativeV :: v -> v
  negativeV a = (-1) */ a
  infixl 6 /+/, /-/
  infixl 7 */

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
