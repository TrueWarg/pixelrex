{-# LANGUAGE Strict            #-}

-------------------------------------------------------------------------------------------
module Pixelrex.Gen.Markov
  ( generateRooms
  , GenRoomsParams(..)
  , OverlappingWeights(..)
  ) where

-------------------------------------------------------------------------------------------
import           Pixelrex.Geometry.Core
import qualified Pixelrex.Geometry.Core          as Geom

import           Control.Monad.Primitive
import           Pixelrex.Core.Point             (Point2D)

import qualified System.Random.MWC               as MWC
import           System.Random.MWC
import           System.Random.MWC.Distributions

import           Control.Monad
import           Control.Monad.ST                (ST, runST)

import           Data.STRef
import           Debug.Trace                     (trace)
import           Pixelrex.Core.Array             (Sz (..), Vector, loopM_, (!),
                                                  (!>))
import           System.Random.Stateful

import qualified Pixelrex.Core.Array             as A

-------------------------------------------------------------------------------------------
data GenRoomsParams =
  GenRoomsParams
    { _roomSizes        :: Sizes2D
    , _roomsCount       :: Int
    , _moves            :: [AffineTransCoef]
    , _weights          :: OverlappingWeights
    , _temperature      :: Double
    , _tempThreshold    :: Double
    , _tempChangeFactor :: Double
    }
  deriving (Eq, Show)

data OverlappingWeights =
  OverlappingWeights
    { _overlapWeight    :: Double
    , _notOverlapWeight :: Double
    , _vTouchWeight     :: Double
    , _hTouchWeight     :: Double
    }
  deriving (Eq, Show)

-------------------------------------------------------------------------------------------
generateRooms ::
     (PrimMonad m) => Gen (PrimState m) -> GenRoomsParams -> m (Vector A.U BBox)
generateRooms gen (GenRoomsParams sizes count moves' weights temperature thThreshold factor) = do
  let initialRooms = initialState sizes count
      moves = A.fromList A.Seq moves'
      go rooms t = do
        newRooms <- updateRooms gen rooms moves weights t
        if (t > thThreshold)
          then go newRooms (t * factor)
          else return rooms
  go initialRooms temperature

-------------------------------------------------------------------------------------------
updateRooms ::
     (PrimMonad m)
  => Gen (PrimState m)
  -> Vector A.U BBox
  -> Vector A.B AffineTransCoef
  -> OverlappingWeights
  -> Double
  -> m (Vector A.U BBox)
updateRooms gen rooms moves weights temperature = do
  uniform <- uniformDoublePositive01M gen
  let (Sz roomSize) = A.size rooms
      (Sz movesSize) = A.size moves
      currentCost = calculateCost rooms weights
      rawProbs =
        A.computeAs A.P $
        A.sfoldl A.sappend A.sempty $
        A.makeArrayR
          A.DS
          A.Seq
          (Sz roomSize)
          (\target ->
             calculateMoveRowProbs
               target
               currentCost
               rooms
               moves
               weights
               temperature)
      sum = A.sum rawProbs
      probs =
        A.makeArrayR
          A.P
          A.Seq
          (Sz (roomSize * movesSize))
          (\i -> (rawProbs ! i) / sum)
      cdf = calculateCdf probs
      sampledIndex =
        case (A.findIndex (> uniform) cdf) of
          Just (A.Ix1 idx) -> idx
          Nothing          -> 0
      roomindex = sampledIndex `div` movesSize
      moveindex = sampledIndex `mod` movesSize
      updatedRooms =
        A.makeArrayR A.U A.Seq (Sz roomSize) $ \i ->
          if (i == roomindex)
            then transform (moves ! moveindex) (rooms ! i)
            else rooms ! i
  return updatedRooms

-------------------------------------------------------------------------------------------
calculateCdf :: Vector A.P Double -> Vector A.P Double
calculateCdf probs = calculateCdf' 0 0.0 A.empty
  where
    (Sz size) = A.size probs
    calculateCdf' idx acc cdf
      | idx < size =
        let sum = acc + (probs ! idx)
         in calculateCdf' (idx + 1) sum (A.snoc cdf sum)
      | otherwise = A.computeAs A.P cdf

{-# INLINE calculateCdf #-}
-------------------------------------------------------------------------------------------
initialState :: Sizes2D -> Int -> A.Vector A.U BBox
initialState sizes n = A.makeArray A.Seq (Sz n) $ \i -> BBox (0, 0) sizes

-------------------------------------------------------------------------------------------
calculateMoveRowProbs ::
     Int
  -> Double
  -> Vector A.U BBox
  -> Vector A.B AffineTransCoef
  -> OverlappingWeights
  -> Double
  -> Vector A.DS Double
calculateMoveRowProbs targetIdx currentCost rooms moves weights t =
  A.smap (f targetIdx) moves
  where
    (Sz size) = A.size rooms
    indices = A.fromList A.Seq [0 .. (size - 1)] :: Vector A.P Int
    f targetIdx move =
      let mapper idx =
            if (idx == targetIdx)
              then Geom.transform move (rooms ! idx)
              else rooms ! idx
          traslated = A.computeAs A.U $ A.smap mapper indices
          cost = calculateCost traslated weights
       in exp (-(cost - currentCost) / t)

-------------------------------------------------------------------------------------------
calculateCost :: Vector A.U BBox -> OverlappingWeights -> Double
calculateCost rooms weights = calculateCost' 0 0
  where
    (OverlappingWeights overlap notOverlap vTouch hTouch) = weights
    Sz size = A.size rooms
    overlapping bbox1 bbox2 =
      case (bboxesOverlaping bbox1 bbox2) of
        (BBoxesAreOverlap (w, h))    -> w * h * overlap
        (BBoxesAreTouchVertical h)   -> h * vTouch
        (BBoxesAreTouchHorizontal w) -> w * hTouch
        (BBoxesAreNotOverlap (w, h)) -> w * h * notOverlap
    calculateCost' i acc
      | i < size = calculateCost' (i + 1) (calculateCostJ' i 0 acc)
      | otherwise = acc
    calculateCostJ' i j acc
      | i == j = calculateCostJ' i (j + 1) acc
      | j < size =
        calculateCostJ' i (j + 1) (acc + overlapping (rooms ! i) (rooms ! j))
      | otherwise = acc
    {-# INLINE overlapping #-}
