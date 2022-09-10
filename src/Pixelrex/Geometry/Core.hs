{-# LANGUAGE FlexibleInstances #-}

-------------------------------------------------------------------------------------------
module Pixelrex.Geometry.Core where

import           Data.Foldable
import           Data.List
import           Pixelrex.Core.Array ((!))
import qualified Pixelrex.Core.Array as A
import           Pixelrex.Core.Point (Point2D)

-------------------------------------------------------------------------------------------
data BBox =
  BBox !(Point2D Double) !(Point2D Double)
  deriving (Eq, Show)

newtype Polygon =
  Polygon (A.Vector A.U (Point2D Double))
  deriving (Eq, Show)

data Line =
  Line !(Point2D Double) !(Point2D Double)
  deriving (Eq, Show)

-------------------------------------------------------------------------------------------
class HasBounds a where
  bounds :: a -> BBox

-------------------------------------------------------------------------------------------
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

instance HasBounds Line where
  bounds (Line start end) = BBox start end

instance HasBounds (Point2D Double) where
  bounds p = BBox p p

instance (HasBounds a) => HasBounds [a] where
  bounds = foldMap bounds

-------------------------------------------------------------------------------------------
{-# INLINE polygonFromBounds #-}
polygonFromBounds :: HasBounds bounded => bounded -> Polygon
polygonFromBounds bounded =
  Polygon $ A.fromList A.Seq [(x1, y1), (x2, y1), (x2, y2), (x1, y2)]
  where
    BBox (x1, y1) (x2, y2) = bounds bounded

-------------------------------------------------------------------------------------------
{-# INLINE isPointInPolygon #-}
isPointInPolygon :: Point2D Double -> Polygon -> Bool
isPointInPolygon point polygon =
  odd (edgeTraversalsCount point (polygonEdges polygon))

-------------------------------------------------------------------------------------------
edgeTraversalsCount :: Foldable f => Point2D Double -> f Line -> Int
edgeTraversalsCount point edges
  | areBBoxesOverlapping point edgesBB = length intersections
  | otherwise = 0
  where
    edges' = toList edges
    edgesBB@(BBox (leftX, _) _) = bounds edges'
    testRay = Line ((leftX - 1), (pointY - 1)) point
    (_, pointY) = point
    intersections = filter (not . areLinesParallel testRay) edges'

-------------------------------------------------------------------------------------------
{-# INLINE areLinesParallel #-}
areLinesParallel :: Line -> Line -> Bool
areLinesParallel lineA lineB = angleCoeff lineA == angleCoeff lineB
  where
    angleCoeff (Line (x1, y1) (x2, y2)) = (y2 - y1) / (x2 - x1)

-------------------------------------------------------------------------------------------
polygonEdges :: Polygon -> A.Vector A.B Line
polygonEdges (Polygon points) =
  let size@(A.Sz szN) = A.size points
      lines =
        A.makeArrayR
          A.B
          A.Par
          size
          (\i ->
             let start = points ! i
                 end =
                   if (i < szN - 1)
                     then points ! i
                     else points ! 0
              in Line start end)
   in lines

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

-------------------------------------------------------------------------------------------
cutPolygon :: Line -> Polygon -> [Polygon]
cutPolygon cuttingLine polyfon = error "Not implemented yet"

-------------------------------------------------------------------------------------------
perpendicularBisector :: Line -> Line
perpendicularBisector line@(Line start end) = error "Not implemented yet"
