{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Data.Massiv.Array
import           Data.Massiv.Array.IO as A
import           Slic.Internal

main :: IO ()
main = do
  image <-
    (A.readImageAuto "C:\\Users\\irrmm\\Desktop\\sample.png") :: IO (A.Image S (SRGB 'NonLinear) Word8)
  let result = process (Params 80 10 5 20) image
      resultPath = "C:\\Users\\irrmm\\Desktop\\result.png"
  putStrLn $ "written: " ++ resultPath
  writeImage resultPath image
  displayImageUsing defaultViewer True . computeAs S =<< concatM 1 [result]
