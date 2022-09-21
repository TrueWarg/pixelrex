module Pixelrex.Geometry.BSDRoom
  ( BSDMeta(..)
  , BSDTree(..)
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
import           Data.Maybe

-------------------------------------------------------------------------------------------
type Coords = Point2D Double
type Padding = Double

data BSDMeta =
  BSDMeta
    { _coords :: Coords
    , _width  :: Double
    , _height :: Double
    , _room   :: Maybe BBox
    , _halls  :: [BBox]
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

data SplitDirection
  = AlongWidth Double
  | AlongHeight Double
  deriving (Eq, Show)

data GenBSDParams
  = GenBSDParams 
  { _minSizes :: Sizes
  , _initialWidth :: Double
  , _initialHeight :: Double
  , _roomPadding :: Double
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
  ration <- uniformDoublePositive01M gen
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
          let split = max minW ((checkNorm ration) * width)
              remaining = width - split
           in if (split >= minW && remaining >= minW) then emptyBSDNode (x, y) split height `children`
              emptyBSDNode (x + split, y) remaining height else SplitResultNone
        AlongHeight ration ->
          let split = max minH ((checkNorm ration) * height)
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
updateRoom :: BSDTree -> BBox -> BSDTree
updateRoom tree room = tree { _bsdMeta = (_bsdMeta tree) { _room = Just room } }

-------------------------------------------------------------------------------------------
generateBSDTree :: (PrimMonad m) => Gen (PrimState m) -> GenBSDParams -> m BSDTree
generateBSDTree gen (GenBSDParams minSizes@(Sizes minW minH) width height padding) = generateBSDTree' gen root
  where
    root = rootBSD width height
    generateBSDTree' gen current
       | widthFromMeta current < minW || heightFromMeta current < minH = do
        let
          (BSDMeta coords w h _ _) = _bsdMeta current 
        room <- generateRoom gen coords w h padding
        return $ updateRoom current room
       | otherwise = do 
        direction <- randomDirection gen
        case (splitBSDNode direction minSizes current) of
          SplitResultChildren left right -> do
            leftSubtree <- generateBSDTree' gen left
            rightSubtree <- generateBSDTree'  gen right
            leftRoom <- getRandomRoom gen leftSubtree
            rightRoom <- getRandomRoom gen rightSubtree
            halls <- generateHall gen leftRoom rightRoom
            let
              meta = (_bsdMeta current) { _halls = halls }
            return $ Node meta leftSubtree rightSubtree
          SplitResultNone -> do
            let
              (BSDMeta coords w h _ _) = _bsdMeta current 
            room <- generateRoom gen coords w h padding
            return $ updateRoom current room

-------------------------------------------------------------------------------------------
generateRoom :: (PrimMonad m) => Gen (PrimState m) -> Coords -> Double -> Double -> Double -> m BBox
generateRoom gen (nodeX, nodeY) width height padding = do
  randW <- uniformDoublePositive01M gen
  randH <- uniformDoublePositive01M gen
  randX <- uniformDoublePositive01M gen
  randY <- uniformDoublePositive01M gen
  let
    maxWidth = width - 2 * padding
    maxHeight = height - 2 * padding
    boxWidth = min maxWidth ((randW + 0.5) * maxWidth)
    boxHeight = min maxHeight ((randH + 0.5) * maxHeight)
    x = nodeX + padding + ((maxWidth - boxWidth) * randX)
    y = nodeY + padding + ((maxHeight - boxHeight) * randY)
  
  return $ BBox (x, y) (x + boxWidth, y + boxHeight)

-------------------------------------------------------------------------------------------
-- todo: make a few halls
generateHall :: (PrimMonad m) => Gen (PrimState m) -> Maybe BBox -> Maybe BBox -> m [BBox]
generateHall _ Nothing _ = return []
generateHall _ _ Nothing = return []
generateHall gen (Just (BBox (lX1, lY1) (lX2, lY2))) (Just (BBox (rX1, rY1) (rX2, rY2))) = do
  leftX <- uniformRM (lX1, lX2) gen
  leftY <- uniformRM (lY1, lY2) gen
  rightX <- uniformRM (rX1, rX2) gen
  rightY <- uniformRM (rY1, rY2) gen
  let
    x1 = max (min leftX rightX) (max lX1 rX1)
    y1 = max (min leftY rightY) (max lY1 rY1)
    x2 = min (max leftX rightX) (min lX2 rX2)
    y2 = min (max leftY rightY) (min lY2 rY2)
  return $ [BBox (x1, y1) (x2, y2)]

-------------------------------------------------------------------------------------------
getRandomRoom :: (PrimMonad m) => Gen (PrimState m) -> BSDTree -> m (Maybe BBox)
getRandomRoom gen tree =
  case (_room $ _bsdMeta tree) of
    room@(Just _) -> return room
    Nothing -> do
      let
        left = _left tree
        right = _right tree

        getFrom left right
         | left /= Empty = getRandomRoom gen left
         | right /= Empty = getRandomRoom gen right
         | left == Empty && right == Empty = return Nothing
         | otherwise = do
          rand <- uniformDoublePositive01M gen
          if (rand < 0.5) then getRandomRoom gen left else getRandomRoom gen right

      getFrom left right
