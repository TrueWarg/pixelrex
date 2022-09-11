{-# LANGUAGE FlexibleInstances #-}

-------------------------------------------------------------------------------------------
module Pixelrex.Geometry.Core where

import           Data.Foldable
import           Data.List
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Set            (Set)
import qualified Data.Set            as S
import           Pixelrex.Core.Array ((!))
import qualified Pixelrex.Core.Array as A
import           Pixelrex.Core.Point (Point2D)

-------------------------------------------------------------------------------------------
type Point = Point2D Double

data BBox =
  BBox !(Point) !(Point)
  deriving (Eq, Show)

newtype Polygon =
  Polygon (A.Vector A.U (Point))
  deriving (Eq, Show)

data PolygonOrientation
  = PolygonPos
  | PolygonNeg
  deriving (Eq, Show)

data Line =
  Line !(Point) !(Point)
  deriving (Eq, Show)

data LineCut
  = NoCut Point Point
  | Cut Point Point Point
  deriving (Eq, Show)

data LineIntersection
  = SegmentIntersection Point
  | InfinityLineIntersectionLeft Point
  | InfinityLineIntersectionRight Point
  | InfinityLineIntersection Point
  | Parallel
  | Collinear
  deriving (Eq, Show)

newtype EdgeGraph =
  EdgeGraph (Map Point (Set Point))
  deriving (Eq, Ord)

data Matrix2D =
  Matrix2D !Double !Double !Double !Double
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

instance HasBounds (Point) where
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
isPointInPolygon :: Point -> Polygon -> Bool
isPointInPolygon point polygon =
  odd (edgeTraversalsCount point (polygonEdges polygon))

-------------------------------------------------------------------------------------------
edgeTraversalsCount :: Foldable f => Point -> f Line -> Int
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
calculateLinesIntersection :: Line -> Line -> LineIntersection
calculateLinesIntersection lineA lineB = error "Not Implemented yet"

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
cutPolygon cuttingLine polygon =
  reconstructPolygons
    (polygonOrientation polygon)
    (createEdgeGraph
       cuttingLine
       (polygonOrientation polygon)
       -- todo: rewrite all function using vector instead list
       (A.toList $ A.map (cutLineWithLine cuttingLine) (polygonEdges polygon)))

-------------------------------------------------------------------------------------------
perpendicularBisector :: Line -> Line
perpendicularBisector line@(Line start end) = error "Not implemented yet"

-------------------------------------------------------------------------------------------
cutLineWithLine :: Line -> Line -> LineCut
cutLineWithLine cuttingLine line =
  case calculateLinesIntersection cuttingLine line of
    SegmentIntersection p           -> cut p
    InfinityLineIntersectionRight p -> cut p
    Collinear                       -> cut start
    _otherwise                      -> noCut
  where
    Line start end = line
    cut p = Cut start p end
    noCut = NoCut start end

-------------------------------------------------------------------------------------------
polygonOrientation :: Polygon -> PolygonOrientation
polygonOrientation polygon
  | signedPolygonArea polygon >= 0 = PolygonPos
  | otherwise = PolygonNeg

-------------------------------------------------------------------------------------------
createEdgeGraph :: Line -> PolygonOrientation -> [LineCut] -> EdgeGraph
createEdgeGraph cuttingLine orientation cuts = error "ot implemented yet"

-------------------------------------------------------------------------------------------
reconstructPolygons :: PolygonOrientation -> EdgeGraph -> [Polygon]
reconstructPolygons orientation graph = error "Not implemented yet"

-------------------------------------------------------------------------------------------
signedPolygonArea :: Polygon -> Double
signedPolygonArea (Polygon points) =
  let size@(A.Sz szN) = A.size points
      determinants =
        A.makeArrayR
          A.B
          A.Par
          size
          (\i ->
             let (x1, y1) = points ! i
                 (x2, y2) =
                   if (i < szN - 1)
                     then points ! i
                     else points ! 0
                 matrix = Matrix2D x1 y1 x2 y2
              in matrix2Determinant matrix)
   in A.sum determinants / 2

-------------------------------------------------------------------------------------------
{-# INLINE matrix2Determinant #-}
matrix2Determinant :: Matrix2D -> Double
matrix2Determinant (Matrix2D a11 a12 a21 a22) = a11 * a22 - a12 * a21
