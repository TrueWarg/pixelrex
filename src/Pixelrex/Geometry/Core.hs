{-# LANGUAGE FlexibleInstances #-}

-------------------------------------------------------------------------------------------
module Pixelrex.Geometry.Core
  ( BBox(..)
  , Polygon(..)
  , PolygonOrientation(..)
  , Segment(..)
  , SegmentIntersection(..)
  , Angle(..)
  , Matrix2D(..)
  , AffineTransCoef(..)
  , AffineTransformation(..)
  , Area(..)
  , HasBounds
  , bounds
  , polygonFromBounds
  , isPointInPolygon
  , edgeTraversalsCount
  , calculateSegmentsIntersection
  , polygonEdges
  , areBBoxesOverlapping
  , polygonOrientation
  , perpendicularBisector
  , matrix2Determinant
  , dotProduct
  , vectorOf
  , cross2D
  , normalizeSegment
  , normalizeAngle
  , rad
  , getRad
  , angleOf
  , boundingBoxCenter
  , isInsideBBox
  ) where

import           Data.Fixed
import           Data.Foldable
import           Data.List
import           Data.Map              (Map)
import qualified Data.Map              as M
import           Data.Set              (Set)
import qualified Data.Set              as S
import           Pixelrex.Core.Algebra
import           Pixelrex.Core.Array   ((!))
import qualified Pixelrex.Core.Array   as A
import           Pixelrex.Core.Point   (Point2D)

-------------------------------------------------------------------------------------------
type Point = Point2D Double

data BBox =
  BBox !(Point) !(Point)
  deriving (Eq, Show)

newtype Polygon =
  Polygon [Point]
  deriving (Eq, Show)

data PolygonOrientation
  = PolygonPos
  | PolygonNeg
  deriving (Eq, Show)

data Segment =
  Segment !(Point) !(Point)
  deriving (Eq, Show)

newtype Angle =
  Rad Double
  deriving (Eq, Show)

-- | M and v in f: R^n -> R^n ~ f(x) = M * x + v
data AffineTransCoef =
  AffineTransCoef !Matrix2D !(Point2D Double)
  deriving (Eq, Show)

data SegmentIntersection
  = RealIntersection Point
  | InfinityLineIntersectionLeft Point
  | InfinityLineIntersectionRight Point
  | InfinityLineIntersection Point
  | Parallel
  | OnSameLine
  deriving (Eq, Show)

data Matrix2D =
  Matrix2D !Double !Double !Double !Double
  deriving (Eq, Show)

-------------------------------------------------------------------------------------------
class HasBounds a where
  bounds :: a -> BBox

-- | f: R^n -> R^n ~ f(x) = M * x + v
class AffineTransformation a where
  transform :: AffineTransCoef -> a -> a

class Area a where
  area :: a -> Double

-------------------------------------------------------------------------------------------
--   * - box A
--   ^ - box B
--   @ - box A <> box B
--   *------            %-------
--   |  ^--|---    ->   |      |
--   |__|__|  |         |      |
--      |__*__|         |______|
--            ^                %
instance Semigroup BBox where
  BBox (xMin1, yMin1) (xMax1, yMax1) <> BBox (xMin2, yMin2) (xMax2, yMax2) =
    BBox
      ((min xMin1 xMin2), (min yMin1 yMin2))
      ((max xMax1 xMax2), (max yMax1 yMax2))

instance Monoid BBox where
  mempty = BBox (inf, inf) (-inf, -inf)
    where
      inf = 1 / 0

instance HasBounds BBox where
  bounds = id

instance HasBounds Segment where
  bounds (Segment start end) = BBox start end

instance HasBounds Point where
  bounds p = BBox p p

instance (HasBounds a) => HasBounds [a] where
  bounds = foldMap bounds

instance VectorSpace Angle where
  Rad a /+/ Rad b = Rad (a + b)
  a */ Rad b = Rad (a * b)

instance AffineTransformation Segment where
  transform t (Segment start end) = Segment (transform t start) (transform t end)

-- Just matrix product
instance Semigroup Matrix2D where
  Matrix2D a11 a12 a21 a22 <> Matrix2D b11 b12 b21 b22 =
    Matrix2D
      (a11 * b11 + a12 * b21)
      (a11 * b12 + a12 * b22)
      (a21 * b11 + a22 * b21)
      (a21 * b12 + a22 * b22)

instance Monoid Matrix2D where
  mempty = Matrix2D 1 0 0 1

instance AffineTransformation b => AffineTransformation (a -> b) where
  transform t f = transform t . f

instance AffineTransformation (Double, Double) where
  transform (AffineTransCoef (Matrix2D a b d e) (c, f)) (x, y) =
    ((a * x + b * y + c), (d * x + e * y + f))

-------------------------------------------------------------------------------------------
polygonFromBounds :: HasBounds bounded => bounded -> Polygon
polygonFromBounds bounded = Polygon [(x1, y1), (x2, y1), (x2, y2), (x1, y2)]
  where
    BBox (x1, y1) (x2, y2) = bounds bounded

{-# INLINE polygonFromBounds #-}
-------------------------------------------------------------------------------------------
isPointInPolygon :: Point -> Polygon -> Bool
isPointInPolygon point polygon =
  odd (edgeTraversalsCount point (polygonEdges polygon))

{-# INLINE isPointInPolygon #-}
-------------------------------------------------------------------------------------------
-- | Counts how many times infinity ray from test point intersects the edges of an object.
-- See `isPointInPolygon` as use case: if counts is odd (for convex polygon is 1), then test point is in polygon,
-- if it is even then point is outer
edgeTraversalsCount ::
     Foldable f
  => Point -- ^ test point
  -> f Segment -- ^ Edges
  -> Int -- ^ Number of edges cross2Ded
edgeTraversalsCount testPoint edges
  | areBBoxesOverlapping testPoint boundingBox = length intersections
  | otherwise = 0
  where
    edges' = toList edges
    boundingBox@(BBox (leftmostX, _) _) = bounds edges'
    (_, pointY) = testPoint
    testRay = Segment ((leftmostX - 1), (pointY - 1)) testPoint
    intersections =
      filter
        (\edge ->
           case calculateSegmentsIntersection testRay edge of
             RealIntersection _ -> True
             _other                -> False)
        edges'

-------------------------------------------------------------------------------------------
-- | See https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
calculateSegmentsIntersection :: Segment -> Segment -> SegmentIntersection
calculateSegmentsIntersection segmentL segmentR = intersectionType
  where
    intersectionType
      | discriminant == 0 && cross2D (v1 /-/ v2) (v1 /-/ v3) /= 0 = Parallel
      | discriminant == 0 = OnSameLine
      | otherwise =
        case (intersectionInsideL, intersectionInsideR) of
          (True, True)   -> RealIntersection intersectionPoint
          (True, False)  -> InfinityLineIntersectionLeft intersectionPoint
          (False, True)  -> InfinityLineIntersectionRight intersectionPoint
          (False, False) -> InfinityLineIntersection intersectionPoint
    Segment v1 v2 = segmentL
    Segment v3 v4 = segmentR
    discriminant = cross2D (v1 /-/ v2) (v3 /-/ v4)
    intersectionPoint =
      (1 / discriminant) */
      (cross2D v1 v2 */ (v3 /-/ v4) /-/ cross2D v3 v4 */ (v1 /-/ v2))
    intersectionInsideL =
      sideOfLine segmentR v1 /= sideOfLine segmentR v2 ||
      sideOfLine segmentR v1 == EQ || sideOfLine segmentR v2 == EQ
    intersectionInsideR =
      sideOfLine segmentL v3 /= sideOfLine segmentL v4 ||
      sideOfLine segmentL v3 == EQ || sideOfLine segmentL v4 == EQ
    sideOfLine :: Segment -> Point -> Ordering
    sideOfLine (Segment u v) p = compare (cross2D (v /-/ u) (p /-/ u)) 0
    forwardness :: Point -> Double
    forwardness v =
      dotProduct (vectorDirection segmentL) (vectorDirection (Segment v1 v))

-------------------------------------------------------------------------------------------
polygonEdges :: Polygon -> [Segment]
polygonEdges (Polygon ps) = zipWith Segment ps (tail (cycle ps))

{-# INLINE polygonEdges #-}
-------------------------------------------------------------------------------------------
areBBoxesOverlapping :: (HasBounds a, HasBounds b) => a -> b -> Bool
areBBoxesOverlapping a b = check (bounds a) (bounds b)
  where
    check (BBox (lowAx, lowAy) (highAx, highAy)) (BBox (lowBx, lowBy) (highBx, highBy))
      | lowAx > highBx = False -- A right of B
      | highAx < lowBx = False -- A left of B
      | lowAy > highBy = False -- A below B
      | highAy < lowBy = False -- A above B
      | otherwise = True

{-# INLINE areBBoxesOverlapping #-}
-------------------------------------------------------------------------------------------
perpendicularBisector :: Segment -> Segment
perpendicularBisector segment@(Segment start end) =
  perpendicularLineThrough middle segment
  where
    middle = 0.5 */ (start /+/ end)

{-# INLINE perpendicularBisector #-}
-------------------------------------------------------------------------------------------
perpendicularLineThrough :: Point -> Segment -> Segment
perpendicularLineThrough point segment@(Segment start@(startX, startY) _) =
  centerSegment segment'
  where
    Segment start0 (endX0, endY0) = transform (translate (-startX, -startY)) segment
    rotatedEnd = ((-endY0), endX0)
    rotated = Segment start0 rotatedEnd
    segment' = transform (translate point) rotated

-------------------------------------------------------------------------------------------
polygonOrientation :: Polygon -> PolygonOrientation
polygonOrientation polygon
  | signedPolygonArea polygon >= 0 = PolygonPos
  | otherwise = PolygonNeg

{-# INLINE polygonOrientation #-}
-------------------------------------------------------------------------------------------
-- | Calculate an area using shoelace formula https://en.wikipedia.org/wiki/Shoelace_formula
signedPolygonArea :: Polygon -> Double
signedPolygonArea (Polygon points) =
  let determinants =
        zipWith
          (\(x1, y1) (x2, y2) -> matrix2Determinant $ Matrix2D x1 y1 x2 y2)
          points
          (tail (cycle points))
   in sum determinants / 2

{-# INLINE signedPolygonArea #-}
-------------------------------------------------------------------------------------------
matrix2Determinant :: Matrix2D -> Double
matrix2Determinant (Matrix2D a11 a12 a21 a22) = a11 * a22 - a12 * a21

{-# INLINE matrix2Determinant #-}
-------------------------------------------------------------------------------------------
dotProduct :: Point -> Point -> Double
dotProduct (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

{-# INLINE dotProduct #-}
-------------------------------------------------------------------------------------------
vectorOf :: Segment -> Point
vectorOf (Segment (startX, startY) (endX, endY)) =
  ((endX - startX), (endY - startY))

{-# INLINE vectorOf #-}
-------------------------------------------------------------------------------------------
normalizeAngle ::
     Angle -- ^ Interval start
  -> Angle -- ^ Angle to normalize
  -> Angle -- ^ Angle normalized to the interval [start, start + deg 360)
normalizeAngle start a = rad (getRad (a /-/ start) `rem'` (2 * pi)) /+/ start
  where
    x `rem'` m = (x `mod'` m + m) `mod'` m

{-# INLINE normalizeAngle #-}
-------------------------------------------------------------------------------------------
deg :: Double -> Angle
deg degrees = Rad (degrees / 180 * pi)

{-# INLINE deg #-}
-------------------------------------------------------------------------------------------
rad :: Double -> Angle
rad = Rad

-------------------------------------------------------------------------------------------
getDeg :: Angle -> Double
getDeg (Rad r) = r / pi * 180

{-# INLINE getDeg #-}
-------------------------------------------------------------------------------------------
getRad :: Angle -> Double
getRad (Rad r) = r

-------------------------------------------------------------------------------------------
cross2D :: Point -> Point -> Double
cross2D (x1, y1) (x2, y2) = matrix2Determinant (Matrix2D x1 y1 x2 y2)

{-# INLINE cross2D #-}
-------------------------------------------------------------------------------------------
vectorDirection :: Segment -> Point
vectorDirection = vectorOf . normalizeSegment

{-# INLINE vectorDirection #-}
-------------------------------------------------------------------------------------------
normalizeSegment :: Segment -> Segment
normalizeSegment = resizeSegment (const 1)

{-# INLINE normalizeSegment #-}
-------------------------------------------------------------------------------------------
resizeSegment :: (Double -> Double) -> Segment -> Segment
resizeSegment f segment@(Segment start _end) =
  let v = vectorOf segment
      len = norm v
      len' = f len
      v' = (len' / len) */ v
      end' = start /+/ v'
   in Segment start end'

{-# INLINE resizeSegment #-}
-------------------------------------------------------------------------------------------
norm :: Point -> Double
norm = sqrt . normSquare

{-# INLINE norm #-}
-------------------------------------------------------------------------------------------
normSquare :: Point -> Double
normSquare v = dotProduct v v

{-# INLINE normSquare #-}
-------------------------------------------------------------------------------------------
angleOf :: Segment -> Angle
angleOf (Segment (x1, y1) (x2, y2)) = rad (atan2 (y2 - y1) (x2 - x1))

{-# INLINE angleOf #-}
-------------------------------------------------------------------------------------------
boundingBoxCenter :: HasBounds a => a -> Point
boundingBoxCenter x =
  let BBox low high = bounds x
   in (1 / 2) */ (low /+/ high)

{-# INLINE boundingBoxCenter #-}
-------------------------------------------------------------------------------------------
isInsideBBox :: (HasBounds thing, HasBounds frame) => thing -> frame -> Bool
isInsideBBox thing frame =
  let thingBBox = bounds thing
      frameBBox = bounds frame
   in frameBBox == frameBBox <> thingBBox

{-# INLINE isInsideBBox #-}
-------------------------------------------------------------------------------------------
centerSegment :: Segment -> Segment
centerSegment segment@(Segment start end) = transform (translate delta) segment
  where
    middle = 0.5 */ (start /+/ end)
    delta = start /-/ middle

{-# INLINE centerSegment #-}
-------------------------------------------------------------------------------------------
translate :: Point -> AffineTransCoef
translate = AffineTransCoef mempty
