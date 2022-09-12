{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Sample
  ( slicSample
  , voronoiSample
  ) where

import           Control.Monad
import           Data.Massiv.Array.IO            as A
import qualified Graphics.Blank                  as Blank
import           Pixelrex                        hiding (forM)
import qualified Pixelrex                        as R
import           System.Random.MWC
import qualified System.Random.MWC               as MWC
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
