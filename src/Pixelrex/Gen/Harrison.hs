-- Simplified implementation of http://wscg.zcu.cz/wscg2001/Papers_2001/R285.pdf
-- A NON-HIERARCHICAL PROCEDURE FOR RE-SYNTHESIS OF COMPLEX TEXTURES, Paul Harrison (2001)
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Strict           #-}

-------------------------------------------------------------------------------------------
module Pixelrex.Gen.Harrison
  ( GenTextureParams(..)
  , reSynthesis
  ) where

-------------------------------------------------------------------------------------------
import           Control.Monad.Primitive
import           Control.Monad.ST                (ST, runST)
import           Data.Maybe                      (fromJust)
import           Data.STRef
import           Debug.Trace                     (trace)
import           Pixelrex.Core.Algebra           (circle0)
import           Pixelrex.Core.Array             (Ix2 (..), MMatrix, Matrix,
                                                  MonadThrow, Sz (..), Vector,
                                                  cons, frameOfMutable,
                                                  indexOfMaxBy, loopM_, patch,
                                                  patchOfMutable, pickValues,
                                                  (!), (!>))
import qualified Pixelrex.Core.Array             as A
import           Pixelrex.Core.Metric            (L1 (..))
import           Pixelrex.Core.Point
import           Pixelrex.Geometry.Core
import qualified Pixelrex.Geometry.Core          as Geom
import qualified System.Random.MWC               as MWC
import           System.Random.MWC
import           System.Random.MWC.Distributions
import           System.Random.Stateful

-------------------------------------------------------------------------------------------
data GenTextureParams =
  GenTextureParams
    { _outputWidth               :: Int
    , _outputHeight              :: Int
    , _epochs                    :: Int -- ^ number of procedure iteration
    , _neighborhood              :: Int -- ^ neighborhood for patches in origin image and output
    , _additionalRandomNeighbors :: Int -- ^ count of pixels which will be chosen randomly
    }

type Image = Matrix A.U (Point3D Float)

-------------------------------------------------------------------------------------------
reSynthesis ::
     (PrimMonad m, MonadThrow m)
  => Gen (PrimState m)
  -> GenTextureParams
  -> Image
  -> m Image
reSynthesis gen (GenTextureParams w h epochs neighborhood additionalRandomNeighbors) sample = do
  indices <- shaffledIndices gen w h
  pickedLocs <- reSynthesis' gen indices 0 initialLocs
  return $ pickPixels sample pickedLocs
  where
    initialLocs = A.makeArrayR A.U A.Seq (Sz (h :. w)) (\_ -> (-1 :. -1))
    reSynthesis' gen indices epoch locs
      | epoch == epochs = return locs
      | otherwise = do
        updated <-
          reSynthesisStep
            gen
            locs
            neighborhood
            additionalRandomNeighbors
            sample
            indices
        reSynthesis' gen indices (epoch + 1) updated

-------------------------------------------------------------------------------------------
pickPixels :: Image -> Matrix A.U Ix2 -> Image
pickPixels sample locs =
  A.makeArrayR
    A.U
    A.Seq
    (A.size locs)
    (\(i :. j) ->
       let (i' :. j') = locs !> i ! j
        in sample !> i' ! j')

{-# INLINE pickPixels #-}
-------------------------------------------------------------------------------------------
reSynthesisStep ::
     (PrimMonad m, MonadThrow m)
  => Gen (PrimState m)
  -> Matrix A.U Ix2
  -> Int
  -> Int
  -> Image
  -> Vector A.U Ix2
  -> m (Matrix A.U Ix2)
reSynthesisStep gen pickedLocs' neighborhood additionalRandomNeighbors sample indices = do
  let Sz n = A.size indices
  pickedLocs <- A.thawS pickedLocs'
  loopM_ 0 (< n) (+ 1) $ \i
    -- it works slow if array too long bacause of copying.
    -- I tried to use mutable version, but probably A.readM for matrix has worse performace than pure analog
    -- of slicing. This is not so critical for such algorytm because it works very slow by design if
    -- image larger than 128x128
    -- todo: try optimaze mutable version (bench linear vector, ask massiv repo maintainer)
   -> do
    frozen <- A.freezeS pickedLocs
    -- todo move maxNeighbors in params
    let maxNeighbors = min 8 i
        idx@(i' :. j') = indices ! i
        neighbors' = collectExistedNeighbors frozen sample maxNeighbors idx
    neighbors'' <- stochasticNeighbors gen additionalRandomNeighbors sample
    let neighbors = A.computeAs A.U $ neighbors' `A.sappend` neighbors''
        neighbor = pickNeighbor frozen idx neighbors sample neighborhood
    A.write pickedLocs (i' :. j') neighbor
  A.freezeS pickedLocs

-------------------------------------------------------------------------------------------
stochasticNeighbors ::
     (PrimMonad m) => Gen (PrimState m) -> Int -> Image -> m (Vector A.DS Ix2)
stochasticNeighbors gen neighborhood sample = stochasticNeighbors' 0 []
  where
    Sz (h :. w) = A.size sample
    stochasticNeighbors' idx neighbors
      | idx == neighborhood = return $ A.sfromList neighbors
      | otherwise = do
        c <- MWC.uniformR (0, h * w - 1) gen
        let i = c `div` h
            j = c `mod` w
        stochasticNeighbors' (idx + 1) ((i :. j) : neighbors)

-------------------------------------------------------------------------------------------
collectExistedNeighbors ::
     Matrix A.U Ix2 -> Image -> Int -> Ix2 -> Vector A.DS Ix2
collectExistedNeighbors pickedLocs sample maxNeighbors center@(ci :. cj) =
  collectExistedNeighbors' 0 1 A.empty
  where
    Sz (h :. w) = A.size pickedLocs
    collectExistedNeighbors' collected radius neighbors
      | collected >= maxNeighbors = A.stake (A.Sz1 maxNeighbors) neighbors
      | (ci + radius >= h) &&
          (ci - radius < 0) && (cj + radius >= w) && (cj - radius < 0) =
        neighbors
      | otherwise =
        let found =
              collectFromFrame
                pickedLocs
                sample
                center
                ((ci - radius) :. (cj - radius))
                ((ci + radius) :. (cj + radius))
            Sz sz = A.size found
            new = neighbors `A.sappend` found
         in collectExistedNeighbors' (collected + sz) (radius + 1) new

-------------------------------------------------------------------------------------------
collectFromFrame ::
     Matrix A.U Ix2 -> Image -> Ix2 -> Ix2 -> Ix2 -> Vector A.U Ix2
collectFromFrame matrix sample (ci :. cj) (y1 :. x1) (y2 :. x2) =
  A.computeAs A.U collected
  where
    collected = collectFromFrame' (y1 + 1) x1 A.empty
    Sz (h :. w) = A.size matrix
    Sz (sh :. sw) = A.size sample
    collectFromFrame' i j frame
      | j < x1 = frame
      | otherwise =
        let i' = circle0 i h
            j' = circle0 j w
            v = matrix !> i' ! j'
            (nextI, nextJ) =
              case (i == y2, j == x2) of
                (False, False) -> if (i == y1) then (i, j - 1) else (i + 1, j)
                (True, False)  -> (i, j + 1)
                (True, True)   -> (i - 1, j)
                (False, True)  -> if (i == y1) then (i, j - 1) else (i - 1, j)
         in case v of
              (-1 :. -1) -> collectFromFrame' nextI nextJ frame
              (vi :. vj) ->
                let center = cj + w * ci
                    neighbor = j' + w * i'
                    picked = vj + sw * vi
                    ni =
                      (picked `div` sw + center `div` w - neighbor `div` w) `mod`
                      sh
                    nj = (picked + (center - neighbor) `mod` w) `mod` sw
                    ni' = circle0 ni sh
                    nj' = circle0 nj sw
                 in collectFromFrame' nextI nextJ ((ni' :. nj') `cons` frame)

-------------------------------------------------------------------------------------------
pickNeighbor :: Matrix A.U Ix2 -> Ix2 -> Vector A.U Ix2 -> Image -> Int -> Ix2
pickNeighbor pickedLocs center neighbors sample neighborhood = neighbors ! idx
  where
    distances =
      A.makeArrayR
        A.U
        A.Seq
        (A.size neighbors)
        (\i ->
           let distance =
                 distanceForPatches
                   pickedLocs
                   center
                   (neighbors ! i)
                   sample
                   neighborhood
            in ((neighbors ! i), distance))
    idx = indexOfMaxBy (\(_, d) -> d) distances

-------------------------------------------------------------------------------------------
distanceForPatches :: Matrix A.U Ix2 -> Ix2 -> Ix2 -> Image -> Int -> Float
distanceForPatches pickedLocs (ci :. cj) (ni :. nj) sample neighborhood =
  distanceForPatches' (-neighborhood) (-neighborhood) 1E-6
  where
    Sz (h :. w) = A.size pickedLocs
    Sz (sh :. sw) = A.size sample
    distanceForPatches' di dj sum
      | dj > neighborhood = distanceForPatches' (di + 1) (-neighborhood) sum
      | di > neighborhood = sum
      | otherwise =
        let ci' = circle0 (ci + di) h
            cj' = circle0 (cj + dj) w
            ni' = circle0 (ni + di) sh
            nj' = circle0 (nj + dj) sw
            idx@(i :. j) = pickedLocs !> ci' ! cj'
            dSum =
              if (idx == (-1 :. -1))
                then 0
                else colorSpaceDistance (sample !> ni' ! nj') (sample !> i ! j)
         in distanceForPatches' di (dj + 1) (sum + dSum)

-------------------------------------------------------------------------------------------
shaffledIndices ::
     (PrimMonad m) => Gen (PrimState m) -> Int -> Int -> m (Vector A.U Ix2)
shaffledIndices gen w h = do
  let n = w * h
  indices <- A.newMArray (Sz n) (0 :. 0)
  loopM_ 0 (< n) (+ 1) $ \c -> do
    r <- MWC.uniformR (0, max 0 (c - 1)) gen
    let (i, j) = (c `div` h, c `mod` h)
    coords <- A.read indices r
    A.write_ indices c $ fromJust coords
    A.write_ indices r (i :. j)
  A.freezeS indices

-------------------------------------------------------------------------------------------
colorSpaceDistance :: Point3D Float -> Point3D Float -> Float
colorSpaceDistance (r1, g1, b1) (r2, g2, b2) = (-1) * (log $ r * g * b)
  where
    colorsCount = 65536
    sigma = 1.0 / (20.0 * colorsCount)
    r = 1 + sigma * ((r2 - r1) ^ 2)
    g = 1 + sigma * ((g2 - g1) ^ 2)
    b = 1 + sigma * ((b2 - b1) ^ 2)

{-# INLINE colorSpaceDistance #-}
