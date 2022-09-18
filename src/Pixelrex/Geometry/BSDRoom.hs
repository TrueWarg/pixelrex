{-# LANGUAGE DataKinds #-}

-------------------------------------------------------------------------------------------
module Pixelrex.Geometry.BSDRoom
  ( BSDMeta(..)
  , BSDTree(..)
  , Hall(..)
  , SplitDirection(..)
  , splitBSDNode
  , emptyBSDLeaf
  ) where

-------------------------------------------------------------------------------------------
import           Pixelrex.Core.Point    (Point2D)
import           Pixelrex.Geometry.Core (BBox)

-------------------------------------------------------------------------------------------
type Coords = Point2D Double

data BSDMeta =
  BSDMeta
    { _coords :: Coords
    , _width  :: Double
    , _height :: Double
    , _room   :: Maybe BBox
    , _halls  :: [Hall]
    }

data BSDTree
  = Node
      { _meta      :: BSDMeta
      , leftChild  :: BSDTree
      , rightChild :: BSDTree
      }
  | Leaf
      { _meta :: BSDMeta
      }

data Hall =
  Hall

data SplitDirection
  = AlongWidth Double
  | AlongHeight Double

-------------------------------------------------------------------------------------------
to :: a -> a -> (a, a)
to elem1 elem2 = (elem1, elem2)

-------------------------------------------------------------------------------------------
splitBSDNode :: SplitDirection -> BSDTree -> (BSDTree, BSDTree)
splitBSDNode direction leaf = splitted
  where
    (BSDMeta (x, y) width height _ _) = _meta leaf
    splitted =
      case direction of
        AlongWidth ration ->
          let split = (checkNorm ration) * width
           in emptyBSDLeaf (x, y) split height `to`
              emptyBSDLeaf (x + split, y) (width - split) height
        AlongHeight ration ->
          let split = (checkNorm ration) * height
           in emptyBSDLeaf (x, y) width split `to`
              emptyBSDLeaf (x, y + split) width (height - split)
    checkNorm ration =
      if (ration < 0.0 || ration > 1.0)
        then error $ "Ration must be in range [0, 1], got " ++ show ration
        else ration

-------------------------------------------------------------------------------------------
emptyBSDLeaf :: Coords -> Double -> Double -> BSDTree
emptyBSDLeaf coords width height = Leaf meta
  where
    meta = BSDMeta coords width height Nothing []
