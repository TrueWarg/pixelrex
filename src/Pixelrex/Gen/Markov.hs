{-# LANGUAGE Strict #-}

module Pixelrex.Gen.Markov
  ( generateRooms
  ) where

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

-- draft impl
generateRooms ::
     (PrimMonad m) => Gen (PrimState m) -> Int -> Double -> m (Vector A.U Box)
generateRooms gen n temperature = do
  let initialRooms = initialState n
      go rooms t = do
        newRooms <- updateRooms gen rooms t
        if (t > 0.1)
          then go newRooms (t * 0.99)
          else return rooms
  go initialRooms temperature

updateRooms ::
     (PrimMonad m)
  => Gen (PrimState m)
  -> Vector A.U Box
  -> Double
  -> m (Vector A.U Box)
updateRooms gen rooms temperature
  -- uniform <- uniformDoublePositive01M gen
 = do
  let uniform = 0.5
      (Sz roomSize) = A.size rooms
      (Sz movesSize) = A.size moves
      currentCost = calculateCost rooms
      rawProbs =
        A.computeAs A.P $
        A.sfoldl A.sappend A.sempty $
        A.makeArrayR
          A.DS
          A.Seq
          (Sz roomSize)
          (\target -> calculateMoveRowProbs target currentCost rooms temperature)
      sum = A.sum rawProbs
      probs = A.makeArrayR 
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

initialState :: Int -> A.Vector A.U Box
initialState n = A.makeArray A.Seq (Sz n) $ \i -> ((0, 0), (5, 5))

moves :: Vector A.B AffineTransCoef
moves =
  A.fromList
    A.Seq
    [ translate (0, 0)
    , translate (8, 0)
    , translate (-8, 0)
    , translate (0, 8)
    , translate (0, -8)
    ]

calculateMoveRowProbs ::
     Int -> Double -> Vector A.U Box -> Double -> Vector A.DS Double
calculateMoveRowProbs targetIdx currentCost rooms t = A.smap (f targetIdx) moves
  where
    (Sz size) = A.size rooms
    indices = A.fromList A.Seq [0 .. (size - 1)] :: Vector A.P Int
    f targetIdx move =
      let mapper idx =
            if (idx == targetIdx)
              then Geom.transform move (rooms ! idx)
              else rooms ! idx
          traslated = A.computeAs A.U $ A.smap mapper indices
          cost = calculateCost traslated
       in exp (-(cost - currentCost) / t)

overlapWeight = 1

notOverlapWeight = 0.01

vTouchWeight = -0.005

hTouchWeight = -0.05

calculateCost :: Vector A.U Box -> Double
calculateCost rooms = calculateCost' 0 0
  where
    Sz size = A.size rooms
    overlapping bbox1 bbox2 =
      case (bboxesOverlaping bbox1 bbox2) of
        (BBoxesAreOverlap (w, h))    -> w * h * overlapWeight
        (BBoxesAreTouchVertical h)   -> h * vTouchWeight
        (BBoxesAreTouchHorizontal w) -> w * hTouchWeight
        (BBoxesAreNotOverlap (w, h)) -> w * h * notOverlapWeight
    calculateCost' i acc
      | i < size = calculateCost' (i + 1) (calculateCostJ' i 0 acc)
      | otherwise = acc
    calculateCostJ' i j acc
      | i == j = calculateCostJ' i (j + 1) acc
      | j < size =
        calculateCostJ' i (j + 1) (acc + overlapping (rooms ! i) (rooms ! j))
      | otherwise = acc
    {-# INLINE overlapping #-}
