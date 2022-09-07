module Pixelrex.Generation.Voronoi where

import           Data.Massiv.Array
import           Data.Massiv.Array   as A
import           Pixelrex.Core.Point

data Point =
  Point
    { order  :: Int
    , coords :: Point2D Int
    , face   :: Face
    }

data Face =
  Face
    { point         :: Point
    , outerHalfEdge :: HalfEdge
    }

type Vertex = Point2D Int

data HalfEdge =
  HalfEdge
    { origin       :: Vertex
    , destination  :: Vertex
    , twin         :: HalfEdge
    , incidentFace :: Face
    , prev         :: HalfEdge
    , next         :: HalfEdge
    }

data Diagram =
  Diagram
    { grid      :: A.Array A.B A.Ix1 Point
    , faces     :: A.Array A.B A.Ix1 Face
    , vertices  :: [Vertex]
    , halfEdges :: [HalfEdge]
    }
