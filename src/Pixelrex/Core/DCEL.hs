module Pixelrex.Core.DCEL where

import qualified Pixelrex.Core.Array as A
import           Pixelrex.Core.Point

data Seed =
  Seed
    { _order  :: Int
    , _coords :: Point2D Int
    , _face   :: Face
    }

data Face =
  Face
    { _seed          :: Seed
    , _outerHalfEdge :: HalfEdge
    }

type Vertex = Point2D Int

data HalfEdge =
  HalfEdge
    { _origin       :: Vertex
    , _destination  :: Vertex
    , _twin         :: HalfEdge
    , _incidentFace :: Face
    , _prev         :: HalfEdge
    , _next         :: HalfEdge
    }

data Diagram =
  Diagram
    { _seeds    :: A.Vector A.B Seed
    , _faces     :: A.Vector A.B Face
    , _vertices  :: [Vertex]
    , _halfEdges :: [HalfEdge]
    }
