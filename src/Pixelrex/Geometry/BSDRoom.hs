{-# LANGUAGE DataKinds #-}

-------------------------------------------------------------------------------------------
module Pixelrex.Geometry.BSDRoom
  ( BSDMeta(..)
  , BSDTree(..)
  , Hall(..)
  , Sizes(..)
  , SplitDirection(..)
  , GenBSDParams(..)
  , SplitResult(..)
  , splitBSDNode
  , emptyBSDNode
  , generateBSDTree
  ) where

-------------------------------------------------------------------------------------------
import           Pixelrex.Core.Point    (Point2D)
import           Pixelrex.Geometry.Core (BBox)
import           System.Random.MWC
import qualified System.Random.MWC               as MWC
import           System.Random.MWC.Distributions
import           Pixelrex.Geometry.Core
import           Control.Monad.Primitive
import           System.Random.Stateful
import           Debug.Trace              (trace)

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
  deriving (Eq, Show)

data BSDTree
  = Node
      { _bsdMeta :: BSDMeta
      , _left    :: BSDTree
      , _right   :: BSDTree
      }
  | Empty
  deriving (Eq, Show)

data Hall =
  Hall
  deriving (Eq, Show)

data SplitDirection
  = AlongWidth Double
  | AlongHeight Double
  deriving (Eq, Show)

data GenBSDParams
  = GenBSDParams 
  { _minSizes :: Sizes
  , _initialWidth :: Double
  , _initialHeight :: Double
  }
  deriving (Eq, Show)

data Sizes = Sizes Double Double
  deriving (Eq, Show)

data SplitResult
  = SplitResultChildren BSDTree BSDTree
  | SplitResultNone
  deriving (Eq, Show)

-------------------------------------------------------------------------------------------

instance Area BSDMeta where
  area (BSDMeta _ w h _ _) = w * h

-------------------------------------------------------------------------------------------

widthFromMeta :: BSDTree -> Double
widthFromMeta (Node meta _ _) = _width meta
widthFromMeta Empty = 0.0

-------------------------------------------------------------------------------------------
heightFromMeta :: BSDTree -> Double
heightFromMeta (Node meta _ _) = _height meta
heightFromMeta Empty = 0.0
-------------------------------------------------------------------------------------------

children :: BSDTree -> BSDTree -> SplitResult
children elem1 elem2 = SplitResultChildren elem1 elem2
-------------------------------------------------------------------------------------------
randomDirection :: (PrimMonad m) => Gen (PrimState m) -> m SplitDirection
randomDirection gen = do
  ration <- uniformDoublePositive01M  gen
  dice <- uniformDoublePositive01M gen
  return $ if (dice <= 0.5) then AlongWidth ration else AlongHeight ration

-------------------------------------------------------------------------------------------
splitBSDNode :: SplitDirection -> Sizes -> BSDTree -> SplitResult
splitBSDNode direction (Sizes minW minH) node = splitted
  where
    (BSDMeta (x, y) width height _ _) = _bsdMeta node
    splitted =
      case direction of
        AlongWidth ration ->
          let split = (checkNorm ration) * width
              remaining = width - split
           in if (split >= minW && remaining >= minW) then emptyBSDNode (x, y) split height `children`
              emptyBSDNode (x + split, y) remaining height else SplitResultNone
        AlongHeight ration ->
          let split = (checkNorm ration) * height
              remaining = height - split
           in if (split >= minH  && remaining >= minH) then emptyBSDNode (x, y) width split `children`
              emptyBSDNode (x, y + split) width remaining else SplitResultNone
    checkNorm ration =
      if (ration < 0.0 || ration > 1.0)
        then error $ "Ratio must be in range [0, 1], got " ++ show ration
        else ration

-------------------------------------------------------------------------------------------
emptyBSDNode :: Coords -> Double -> Double -> BSDTree
emptyBSDNode coords width height = Node meta Empty Empty
  where
    meta = BSDMeta coords width height Nothing []

-------------------------------------------------------------------------------------------
rootBSD ::  Double -> Double -> BSDTree
rootBSD width height = emptyBSDNode (0, 0) width height

-------------------------------------------------------------------------------------------

generateBSDTree :: (PrimMonad m) => Gen (PrimState m) -> GenBSDParams -> m BSDTree
generateBSDTree gen (GenBSDParams minSizes@(Sizes minW minH) width height) = generateBSDTree' gen root
  where
    root = rootBSD width height
    generateBSDTree' gen current
       | widthFromMeta current < minW || heightFromMeta current < minH = return current
       | otherwise = do 
        direction <- randomDirection gen
        case (splitBSDNode direction minSizes current) of
          SplitResultChildren left right -> do
            leftSubtree <- generateBSDTree' gen left
            rightSubtree <- generateBSDTree'  gen right
            
            return $ Node (_bsdMeta current) leftSubtree rightSubtree
          SplitResultNone -> return current
