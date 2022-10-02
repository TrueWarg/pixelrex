{-# LANGUAGE BangPatterns         #-}

module Pixelrex.Gen.Markov where

import           Pixelrex.Geometry.Core
import qualified Pixelrex.Geometry.Core as Geom

import           Pixelrex.Core.Point    (Point2D)
import           Control.Monad.Primitive

import           System.Random.MWC
import qualified System.Random.MWC               as MWC
import           System.Random.MWC.Distributions

import           Control.Monad
import           Control.Monad.ST (ST, runST)

import           System.Random.Stateful
import           Data.STRef
import           Debug.Trace              (trace)
import           Pixelrex.Core.Array           (Sz (..), loopM_, (!), (!>), Vector)

import qualified Pixelrex.Core.Array           as A
-- draft impl

generateRooms :: (PrimMonad m) => Gen (PrimState m) -> Int -> Double -> m (Vector A.B BBox)
generateRooms gen n temperature = do
  initialRooms <- initialState gen n
  let
    go rooms !t = do
      newRooms <- updateRooms gen rooms t
      if (t > 0.1) then
        go newRooms (t*0.99)
      else return rooms
  go initialRooms temperature

updateRooms :: (PrimMonad m) => Gen (PrimState m) -> Vector A.B BBox -> Double -> m (Vector A.B BBox)
updateRooms gen rooms temperature = do
  uniform <- uniformDoublePositive01M gen
  let
    (Sz roomSize) = A.size rooms
    (Sz movesSize) = A.size moves
    currentCost = calculateCost(rooms)
    indices = A.fromList A.Par [0.. (roomSize - 1)] :: Vector A.S Int
    rawProbs =
      A.sfoldl A.sappend A.sempty $
      A.smap
        (\target -> calculateMoveRowProbs target currentCost rooms temperature)
        indices

    sum = A.ssum rawProbs
    probs = A.computeAs A.S $ A.smap (/ sum) rawProbs
    cdf = calculateCdf probs
    sampledIndex = case (A.findIndex (> uniform) cdf) of
      Just (A.Ix1 idx) -> idx
      Nothing -> 0

    roomindex = sampledIndex `div` movesSize
    moveindex = sampledIndex `mod` movesSize
    updatedRooms =
      A.makeArrayR A.B A.Par (Sz roomSize) $ \i ->
        if (i == roomindex)
          then transform (moves ! moveindex) (rooms ! i)
          else rooms ! i

  return updatedRooms

calculateCdf :: Vector A.S Double -> Vector A.S Double
calculateCdf probs = runST $ go probs
  where
    go probs = do
      let
        sz@(Sz size) = A.size probs
      cdf <- A.newMArray sz (0.0)
      accRef <- newSTRef (0.0)
      loopM_ 0 (< size) (+ 1) $ \i -> do
        acc <- readSTRef accRef
        A.write_ cdf i ((probs ! i) + acc)
        modifySTRef' accRef (+ (probs ! i))
        return ()

      A.freezeS cdf

initialState :: (PrimMonad m) => Gen (PrimState m) -> Int -> m (Vector A.B BBox)
initialState gen n = do
    generated <- A.sreplicateM (Sz n) $ uniformRM (10, 10::Int) gen
    let
      bboxes = A.smap (\p -> BBox (0, 0) (fromIntegral p, fromIntegral p)) generated
    return $ A.computeAs A.B $ bboxes

moves :: Vector A.B AffineTransCoef
moves = A.fromList A.Par
 [ translate (0, 0)
 , translate (8, 0)
 , translate (-8, 0)
 , translate (0, 8)
 , translate (0, -8)
 ]

calculateMoveRowProbs :: Int -> Double -> Vector A.B BBox -> Double -> Vector A.DS Double
calculateMoveRowProbs targetIdx currentCost rooms t = A.smap (f targetIdx) moves
  where
    (Sz size) = A.size rooms
    indices = A.fromList A.Par [0.. (size - 1)] :: Vector A.S Int
    f targetIdx move = 
      let
        mapper idx = if (idx == targetIdx) then Geom.transform move (rooms ! idx) else rooms ! idx
        traslated = A.computeAs A.B $ A.smap mapper indices
        cost = calculateCost traslated
      in exp (- (cost - currentCost) / t)

calculateCost :: Vector A.B BBox -> Double
calculateCost rooms = foldr fun 0 zipped
  where
    overlapWeight = 1
    notOverlapWeight = 0.01
    vTouchWeight = -0.005
    hTouchWeight = -0.05

    Sz size = A.size rooms
    indices = [0..size - 1]
    is = concat $ map (replicate size) indices

    js = concat $ replicate size indices
    zipped = zip is js
    fun (i, j) acc = if (i == j) then acc else
      case (bboxesOverlaping (rooms ! i) (rooms ! j)) of
        (BBoxesAreOverlap (Sizes2D w h)) -> acc + w * h * overlapWeight 
        (BBoxesAreTouchVertical h) -> acc + h * vTouchWeight
        (BBoxesAreTouchHorizontal w) -> acc + w * hTouchWeight
        (BBoxesAreNotOverlap (Sizes2D w h)) -> acc + w * h * notOverlapWeight
