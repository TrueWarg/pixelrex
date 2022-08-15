{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Data.Massiv.Array.IO as A
import           Pixelrex
import qualified Pixelrex             as R

main :: IO ()
main = do
  image <-
    (A.readImageAuto "sample.jpg") :: IO (A.Image S (SRGB 'NonLinear) Word8)
  let labImage =
        (computeAs S $ convertImage image) :: (A.Image S (LAB D65) Float)
      result = process (Params 250 3 45 10) (convertToRexFormat labImage)
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
