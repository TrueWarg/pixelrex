{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}

module Sample
  ( slicSample
  , voronoiSample
  , bsdRoomSample
  , markovRoomsSample
  ) where

import           Control.Monad
import           Data.Massiv.Array.IO            as A
import qualified Graphics.Blank                  as Blank
import           Pixelrex                        hiding (forM, zip)
import qualified Pixelrex                        as R
import           System.Random.MWC
import qualified System.Random.MWC               as MWC
import           System.Random.MWC.Distributions
import     qualified      Data.Text                       as T
import           Data.Maybe
import           Debug.Trace              (trace)

slicSample :: IO ()
slicSample = do
  image <-
    (A.readImageAuto "sample.png") :: IO (A.Image S (SRGB 'NonLinear) Word8)
  let labImage =
        (computeAs S $ convertImage image) :: (A.Image S (LAB D65) Float)
      params =
        Params
          { clusterLength = 13
          , stride = 2
          , iterations = 5
          , spaceDistanceWeight = 10
          }
      result = processSlic params (convertToRexFormat labImage)
      resultPath = "result.png"
  putStrLn $ "written: " ++ resultPath
  writeImage resultPath image
  displayImageUsing defaultViewer True . computeAs S =<<
    concatM 1 [convertToSealab result]

convertToRexFormat :: (A.Image S (LAB D65) Float) -> R.Image Float
convertToRexFormat image =
  makeArrayR
    U
    Par
    (size image)
    (\(i :. j) ->
       let (Pixel (ColorLAB l a b)) = image !> i ! j
        in (l, a, b))

convertToSealab :: R.Image Float -> (A.Image S (LAB D65) Float)
convertToSealab image =
  makeArrayR
    S
    Par
    (size image)
    (\(i :. j) ->
       let (l, a, b) = image !> i ! j
        in (Pixel (ColorLAB l a b)))

voronoiSample :: IO ()
voronoiSample = do
  points <-
    liftIO $ do
      gen <- MWC.create
      gaussianDistributedPoints
        gen
        (BBox (0.0, 0.0) (600.0, 400.0))
        (Matrix2D 150.0 0.0 0.0 100.0)
        200
  let diagram =
        createVoronoi
          (BBox (0.0, 0.0) (600.0, 400.0))
          (fmap (\point -> (point, ())) points)
  Blank.blankCanvas 3000 $ \context -> do
    Blank.send context $ do
      R.forM_ (_cells diagram) $ \cell -> do
        let (CellMeta (x, y) polygon) = _meta cell
        forM (polygonEdges polygon) $ \line -> do
          let (Segment (startX, startY) (endX, endY)) = line
          Blank.moveTo (startX, startY)
          Blank.lineTo (endX, endY)
          Blank.lineWidth 1
          Blank.strokeStyle "blue"
          Blank.stroke ()

gaussianDistributedPoints ::
     (PrimMonad m, HasBounds boundingBox)
  => Gen (PrimState m)
  -> boundingBox
  -> Matrix2D
  -> Int
  -> m (R.Vector R.DS (Point2D Double))
gaussianDistributedPoints gen container covariance count =
  R.sreplicateM (R.Sz count) randomPoint
  where
    bb = bounds container
    center = boundingBoxCenter bb
    randomPoint = do
      let t = AffineTransCoef covariance center
      vec <- transform t . (,) <$> standard gen <*> standard gen
      if vec `isInsideBBox` bb
        then pure vec
        else randomPoint

bsdRoomSample :: IO ()
bsdRoomSample = do
  let
    params = GenBSDParams (Sizes 10 10) 900 600 2
  gen <- createSystemRandom
  tree <- generateBSDTree gen params
  let
    metas = leafMetas tree
    halls = getHalls tree
  Blank.blankCanvas 3000 $ \context -> do
    Blank.send context $ do
      forM (zip [0..] halls) $ \(idx, (BBox (x1, y1) (x2, y2))) -> do
        Blank.beginPath()
        Blank.moveTo(x1, y1)
        Blank.lineTo(x1, y2)
        Blank.lineTo(x2, y2)
        Blank.lineTo(x2, y1)
        Blank.closePath()
        Blank.fillStyle "black"
        Blank.lineWidth 1
        Blank.fill()

      forM (zip [0..] metas) $ \(idx, meta) -> do
        let (BSDMeta (x, y) w h room _) = meta
        case room of
          (Just (BBox (x1, y1) (x2, y2))) -> do
            Blank.beginPath()
            Blank.moveTo(x1, y1)
            Blank.lineTo(x1, y2)
            Blank.lineTo(x2, y2)
            Blank.lineTo(x2, y1)
            Blank.globalAlpha 1
            Blank.closePath()
            Blank.fillStyle "black"
            Blank.lineWidth 1
            Blank.fill()
          Nothing -> return ()
    return ()

markovRoomsSample :: IO ()
markovRoomsSample = do
  gen <- createSystemRandom
  rooms <- generateRooms gen 30 250
  putStrLn (show $ R.size rooms)
  let
    shiftX = 600
    shiftY = 250
  Blank.blankCanvas 3000 $ \context -> do
    Blank.send context $ do
      forM (zip [0..] (R.toList rooms)) $ \(idx, (BBox (x1, y1) (x2, y2))) -> do
        Blank.beginPath()
        Blank.moveTo(x1 + shiftX, y1 + shiftY)
        Blank.lineTo(x1 + shiftX, y2 + shiftY)
        Blank.lineTo(x2 + shiftX, y2 + shiftY)
        Blank.lineTo(x2 + shiftX, y1 + shiftY)
        Blank.closePath()
        Blank.fillStyle (color idx)
        Blank.lineWidth 1
        Blank.fill()
    return ()

generateRooms' gen n temperature = do
  initialRooms <- initialState gen n
  let
    shiftX = 600
    shiftY = 250
    go rooms !t = do
      newRooms <- updateRooms gen rooms t
      Blank.blankCanvas 3000 $ \context -> do
        Blank.send context $ do
          forM (zip [0..] (R.toList rooms)) $ \(idx, (BBox (x1, y1) (x2, y2))) -> do
           Blank.beginPath()
           Blank.moveTo(x1 + shiftX, y1 + shiftY)
           Blank.lineTo(x1 + shiftX, y2 + shiftY)
           Blank.lineTo(x2 + shiftX, y2 + shiftY)
           Blank.lineTo(x2 + shiftX, y1 + shiftY)
           Blank.closePath()
           Blank.fillStyle (color idx)
           Blank.lineWidth 1
           Blank.fill()
        return ()
      if (t > 0.1) then
        go newRooms (t * 0.99)
      else return rooms
  go initialRooms temperature
  return ()

color :: Int -> T.Text
color idx = colors !! (idx `mod` 6)
  where
    colors = ["red", "blue", "green", "orange", "black", "orange"]

leafMetas :: BSDTree -> [BSDMeta]
leafMetas tree = leaves' [] [tree]
  where
    leaves' ls [] = ls
    leaves' ls (node : queue) = 
      case node of
        (Node meta left right) -> if (left == Empty && right == Empty) 
          then leaves' (_bsdMeta node : ls) queue 
          else leaves' ls (left : right : queue) 
        Empty -> leaves' ls queue

getHalls :: BSDTree -> [BBox]
getHalls tree = halls' [] [tree]
  where
    halls' hs [] = hs
    halls' hs (node : queue) =
      case node of
        (Node meta left right) -> 
          let 
             halls = _halls $ _bsdMeta node 
          in halls' (halls <> hs) (left : right : queue) 
        Empty -> halls' hs queue