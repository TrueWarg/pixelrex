{-# LANGUAGE TypeFamilies #-}

-------------------------------------------------------------------------------------------
module Pixelrex.Geometry.Voronoi where

import qualified Pixelrex.Core.Array       as A
import           Pixelrex.Core.Point       (Point2D)

import           Data.List
import           Pixelrex.Core.FunctorMeta
import           Pixelrex.Geometry.Core

-------------------------------------------------------------------------------------------
type Seed = Point2D Double

data CellMeta =
  CellMeta
    { _seed    :: Seed
    , _polygon :: Polygon
    }
  deriving (Eq, Show)

data Cell a =
  Cell
    { _meta    :: CellMeta
    , _payload :: a
    }
  deriving (Eq, Show)

data Diagram a =
  Diagram
    { _bbox  :: BBox
    , _cells :: A.Vector A.B (Cell a)
    }
  deriving (Eq, Show)

-------------------------------------------------------------------------------------------
instance Functor Cell where
  fmap f cell@(Cell _ payload) = cell {_payload = f payload}

instance Functor Diagram where
  fmap f diagram@(Diagram _ cells) =
    diagram {_cells = A.computeAs A.B $ A.smap (fmap f) cells}

instance FunctorMeta Cell where
  type Meta Cell = CellMeta
  fmapWithMeta f cell@(Cell meta payload) = cell {_payload = f meta payload}

instance FunctorMeta Diagram where
  type Meta Diagram = CellMeta
  fmapWithMeta f diagram@(Diagram _ cells) =
    let newCells =
          A.computeAs A.B $
          A.smap
            (\cell@(Cell meta payload) -> cell {_payload = f meta payload})
            cells
     in diagram {_cells = newCells}

-------------------------------------------------------------------------------------------
{-# INLINE createVoronoi #-}
createVoronoi ::
     (Foldable f, HasBounds bounded) => bounded -> f (Seed, a) -> Diagram a
createVoronoi frame = foldl' addPoint (emptyVoronoi frame)

-------------------------------------------------------------------------------------------
{-# INLINE emptyVoronoi #-}
emptyVoronoi :: HasBounds bounded => bounded -> Diagram a
emptyVoronoi world = Diagram (bounds world) A.empty

-------------------------------------------------------------------------------------------
addPoint :: Diagram a -> (Seed, a) -> Diagram a
addPoint (Diagram box cells) (seed, a) = Diagram box (A.computeAs A.B cells')
  where
    meta = CellMeta seed (polygonFromBounds box)
    newCell =
      foldl'
        (\cell (Cell meta _) -> updateCell (_seed meta) cell)
        (Cell meta a)
        cells
    -- todo: find way no call computeAs two times. (probably create Source Ds Cell a instance is needed to use A.append')
    cells' =
      A.cons
        newCell
        (A.computeAs A.B $ A.smap (updateCell $ _seed $ _meta newCell) cells)

-------------------------------------------------------------------------------------------
updateCell :: Seed -> Cell a -> Cell a
updateCell seed cell = clipCell (perpendicularBisector (Line (_seed meta) seed)) cell
  where
    meta = _meta cell

-------------------------------------------------------------------------------------------
clipCell :: Line -> Cell a -> Cell a
clipCell line cell =
  case filter (isPointInPolygon (_seed meta)) (cutPolygon line (_polygon meta)) of
    [polygon] -> cell {_meta = meta {_polygon = polygon}}
    otherwiseResult ->
      error $ "Expected 1 cell, got " ++ (show $ length otherwiseResult)
  where
    meta = _meta cell
