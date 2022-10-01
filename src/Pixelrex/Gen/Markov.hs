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
        go newRooms (t * 0.99)
      else return rooms
  go initialRooms temperature

updateRooms :: (PrimMonad m) => Gen (PrimState m) -> Vector A.B BBox -> Double -> m (Vector A.B BBox)
updateRooms gen rooms temperature = do
  uniform <- uniformDoublePositive01M gen
  let
    (Sz roomSize) = A.size rooms
    (Sz movesSize) = A.size moves
    currentCost = volumeCostFunction(rooms)
    rawProbs =
      A.sfoldl A.sappend A.sempty $
      A.smap
        (\room -> calculateMoveRowProbs room currentCost rooms temperature)
        rooms

    sum = A.ssum rawProbs
    probs = A.computeAs A.S $ A.smap (/ sum) rawProbs
    cdf = calculateCdf probs
    sampledIndex = case (A.findIndex (> uniform) cdf) of
      Just (A.Ix1 idx) -> idx
      Nothing -> 0
    roomindex = sampledIndex `div` roomSize

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
        modifySTRef' accRef (+ (probs ! i))
        acc <- readSTRef accRef
        A.write_ cdf i((probs ! i) + acc)
        return ()

      A.freezeS cdf

initialState :: (PrimMonad m) => Gen (PrimState m) -> Int -> m (Vector A.B BBox)
initialState gen n = do
    generated <- A.sreplicateM (Sz n) $ uniformRM (20, 50::Int) gen
    let
      bboxes = A.smap (\p -> BBox (0, 0) (fromIntegral p, fromIntegral p)) generated
    return $ A.computeAs A.B $ bboxes

moves :: Vector A.B AffineTransCoef
moves = A.fromList A.Par
 [ translate (0, 0)
 , translate (1, 0)
 , translate (-1, 0)
 , translate (0, 1)
 , translate (0, -1)
 ]

calculateMoveRowProbs :: BBox -> Double -> Vector A.B BBox -> Double -> Vector A.DS Double
calculateMoveRowProbs target currentCost rooms t = A.smap (f target) moves
  where
    f target move = 
      let
        mapper bbox = if (bbox == target) then Geom.transform move bbox else bbox
        traslated = A.computeAs A.B $ A.smap mapper rooms
      in exp (- (volumeCostFunction(rooms) - currentCost) / t)

volumeCostFunction :: Vector A.B BBox -> Double
volumeCostFunction rooms = A.sfoldl fun 0 zipped
  where
    overlapWeight = -4
    notOverlapWeight = 4
    vTouchWeight = -6
    hTouchWeight = -6

    size = A.size rooms
    is = A.sfoldl A.sappend A.sempty (A.smap (A.sreplicate size) rooms)
    js = A.sfoldl A.sappend A.sempty (A.sreplicate size rooms)
    zipped = A.szip is js
    fun acc (box1, box2) =
      case (bboxesOverlaping box1 box2) of
        (BBoxesAreOverlap (Sizes2D w h)) -> acc + (abs $ w * h) * overlapWeight 
        (BBoxesAreTouchVertical h) -> acc + h * vTouchWeight + (area box1)
        (BBoxesAreTouchHorizontal w) -> acc + w * hTouchWeight + (area box1)
        (BBoxesAreNotOverlap (Sizes2D w h)) -> acc + (abs $ w * h) * notOverlapWeight
