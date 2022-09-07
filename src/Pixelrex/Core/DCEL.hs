module Pixelrex.Core.DCEL where

import           Pixelrex.Core.Array as A
import           Pixelrex.Core.Point

data Seed =
  Seed
    { order  :: Int
    , coords :: Point2D Int
    , face   :: Face
    }

data Face =
  Face
    { seed          :: Seed
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
    { seeds    :: A.Array A.B A.Ix1 Seed
    , faces     :: A.Array A.B A.Ix1 Face
    , vertices  :: [Vertex]
    , halfEdges :: [HalfEdge]
    }
