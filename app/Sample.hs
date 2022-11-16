{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Sample
  ( slicSample
  , voronoiSample
  , bsdRoomSample
  , markovRoomsSample
  , harrisonSample
  , frameOfMutable'
  , indexOfMaxBy'
  ) where

import           Control.Monad
import           Data.Massiv.Array.IO            as A
import           Data.Maybe
import qualified Data.Text                       as T
import           Data.Time.Clock.POSIX
import qualified Data.Vector                     as V
import qualified Data.Vector.Generic             as VG
import           Debug.Trace                     (trace)
import qualified Graphics.Blank                  as Blank
import           Pixelrex                        hiding (forM, zip)
import qualified Pixelrex                        as R
import qualified System.Random.MWC               as MWC
import           System.Random.MWC
import           System.Random.MWC.Distributions

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
  writeImage resultPath (convertToSealab result)
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
  let params = GenBSDParams (Sizes 10 10) 900 600 2
  gen <- createSystemRandom
  tree <- generateBSDTree gen params
  let metas = leafMetas tree
      halls = getHalls tree
  Blank.blankCanvas 3000 $ \context -> do
    Blank.send context $ do
      forM (zip [0 ..] halls) $ \(idx, (BBox (x1, y1) (x2, y2))) -> do
        Blank.beginPath ()
        Blank.moveTo (x1, y1)
        Blank.lineTo (x1, y2)
        Blank.lineTo (x2, y2)
        Blank.lineTo (x2, y1)
        Blank.closePath ()
        Blank.fillStyle "black"
        Blank.lineWidth 1
        Blank.fill ()
      forM (zip [0 ..] metas) $ \(idx, meta) -> do
        let (BSDMeta (x, y) w h room _) = meta
        case room of
          (Just (BBox (x1, y1) (x2, y2))) -> do
            Blank.beginPath ()
            Blank.moveTo (x1, y1)
            Blank.lineTo (x1, y2)
            Blank.lineTo (x2, y2)
            Blank.lineTo (x2, y1)
            Blank.globalAlpha 1
            Blank.closePath ()
            Blank.fillStyle "black"
            Blank.lineWidth 1
            Blank.fill ()
          Nothing -> return ()
    return ()

markovRoomsSample :: IO ()
markovRoomsSample = do
  gen <- createSystemRandom
  start <- timeMillis
  putStrLn $ "Start " ++ show start
  let moves =
        [ translate (0, 0)
        , translate (48, 0)
        , translate (-48, 0)
        , translate (0, 48)
        , translate (0, -48)
        ]
      weights = OverlappingWeights 1 0.01 (-0.005) (-0.05)
      params = GenRoomsParams (30, 30) 60 moves weights 250 0.1 0.99
  rooms <- generateRooms gen params
  end <- timeMillis
  putStrLn $ show (end - start)
  let shiftX = 600
      shiftY = 250
  Blank.blankCanvas 3000 $ \context -> do
    Blank.send context $ do
      forM (zip [0 ..] (R.toList rooms)) $ \(idx, (BBox (x1, y1) (x2, y2))) -> do
        Blank.beginPath ()
        Blank.moveTo (x1 + shiftX, y1 + shiftY)
        Blank.lineTo (x1 + shiftX, y2 + shiftY)
        Blank.lineTo (x2 + shiftX, y2 + shiftY)
        Blank.lineTo (x2 + shiftX, y1 + shiftY)
        Blank.closePath ()
        Blank.fillStyle (color idx)
        Blank.lineWidth 1
        Blank.fill ()
    return ()

timeMillis :: Integral b => IO b
timeMillis =
  getCurrentTime >>= pure . (1000 *) . utcTimeToPOSIXSeconds >>= pure . round

color :: Int -> T.Text
color idx = colors !! (idx `mod` 6)
  where
    colors = ["red", "blue", "green", "orange", "black", "orange"]

leafMetas :: BSDTree -> [BSDMeta]
leafMetas tree = leaves' [] [tree]
  where
    leaves' ls [] = ls
    leaves' ls (node:queue) =
      case node of
        (Node meta left right) ->
          if (left == Empty && right == Empty)
            then leaves' (_bsdMeta node : ls) queue
            else leaves' ls (left : right : queue)
        Empty -> leaves' ls queue

getHalls :: BSDTree -> [BBox]
getHalls tree = halls' [] [tree]
  where
    halls' hs [] = hs
    halls' hs (node:queue) =
      case node of
        (Node meta left right) ->
          let halls = _halls $ _bsdMeta node
           in halls' (halls <> hs) (left : right : queue)
        Empty -> halls' hs queue

harrisonSample :: IO ()
harrisonSample = do
  image <-
    (A.readImageAuto "sample.png") :: IO (A.Image S (SRGB 'NonLinear) Word8)
  -- gen <- MWC.initialize ((VG.singleton 42) :: V.Vector Word32)
  gen <- createSystemRandom
  start <- timeMillis
  let params =
        GenTextureParams
          { _outputWidth = 128
          , _outputHeight = 128
          , _epochs = 4
          , _neighborhood = 4
          , _additionalRandomNeighbors = 10
          }
      resultPath = "result.png"
  putStrLn "start"
  result <- reSynthesis gen params (convertToRexFormat' image)
  end <- timeMillis
  putStrLn $ show (end - start)
  putStrLn $ "written: " ++ resultPath
  writeImage resultPath (convertToSRGB result)
  displayImageUsing defaultViewer True . computeAs S =<<
    concatM 1 [convertToSRGB result]

convertToRexFormat' :: (A.Image S (SRGB 'NonLinear) Word8) -> R.Image Float
convertToRexFormat' image =
  makeArrayR
    U
    Par
    (size image)
    (\(i :. j) ->
       let (Pixel (ColorSRGB r g b)) = image !> i ! j
        in (fromIntegral r, fromIntegral g, fromIntegral b))
        
convertToSRGB :: R.Image Float -> (A.Image S (SRGB 'NonLinear) Word8)
convertToSRGB image =
  makeArrayR
    S
    Par
    (size image)
    (\(i :. j) ->
       let (r, g, b) = image !> i ! j
        in (Pixel (ColorSRGB (round r) (round g) (round b))))

submatrix' = do
  let matrix = R.makeArrayR R.U R.Par (R.Sz (5 :. 5)) (\(i :. j) -> i :. j)
      submatrix = R.submatrix matrix (1 :. 1) (3 :. 4)
  putStrLn $ show matrix
  putStrLn $ "------------------------------------"
  putStrLn $ show submatrix

mutSubmatrix' :: IO ()
mutSubmatrix' = do
  matrix <-
    R.newMArray (R.Sz (5 :. 5)) (1) :: IO (MArray (PrimState IO) R.U Ix2 Int)
  submatrix <- R.submatrixOfMutable matrix (1 :. 1) (3 :. 4)
  -- putStrLn $ show matrix
  putStrLn $ "------------------------------------"
  putStrLn $ show submatrix

findMaxBy = do
  let arr = R.makeArrayR R.U R.Par (R.Sz1 3) (\i -> (i, 0 :: Int))
  putStrLn $ show arr
  putStrLn $ "------------------------------------"
  putStrLn $ show $ R.maxBy (\(i, _) -> i) arr

indexOfMaxBy' = do
  let arr =
        R.fromList
          R.Seq
          [ (0 :. 1, 9.926532e-7)
          , (0 :. 0, 4.926531e-7)
          , (0 :. 0, 4.926531e-7)
          , (0 :. 0, 4.926531e-7)
          , (0 :. 0, 4.926531e-7)
          , (0 :. 0, 4.926531e-7)
          , (0 :. 0, 4.926531e-7)
          , (0 :. 0, 4.926531e-7)
          , (0 :. 0, 4.926531e-7)
          , (0 :. 0, 4.926531e-7)
          , (0 :. 0, 4.926531e-7)
          ] :: R.Vector R.U (R.Ix2, Float)
  putStrLn $ show arr
  putStrLn $ "------------------------------------"
  putStrLn $ show $ R.indexOfMaxBy (\(i, _) -> i) arr

pickValues' = do
  let matrix =
        R.makeArrayR
          R.B
          R.Par
          (R.Sz (5 :. 5))
          (\(i :. j) -> show i ++ " / " ++ show j)
      indices =
        R.makeArrayR R.U R.Par (R.Sz (3 :. 3)) (\(i :. j) -> (i + 1) :. (j + 1))
      values = R.pickValues matrix indices
  putStrLn $ show matrix
  putStrLn $ "------------------------------------"
  putStrLn $ show indices
  putStrLn $ "------------------------------------"
  putStrLn $ show values

frameOfMutable' :: IO ()
frameOfMutable' = do
  matrix <-
    R.newMArray (R.Sz (5 :. 5)) (0 :. 0) :: IO (MArray (PrimState IO) R.U Ix2 Ix2)
  let matrixkek = R.makeArrayR R.U R.Par (R.Sz (5 :. 5)) (\(i :. j) -> i :. j)
  loopM_ 0 (< 5) (+ 1) $ \i -> do
    loopM_ 0 (< 5) (+ 1) $ \j -> do R.write_ matrix (i :. j) (i :. j)
  frame <- R.frameOfMutable matrix (0 - 3 :. 0 - 3) (3 :. 3)
  putStrLn $ show matrixkek
  putStrLn $ "------------------------------------"
  putStrLn $ show frame
