module Pixelrex.Gen.Harrison
  (
  ) where

import           Control.Monad.Primitive
import           Control.Monad.ST                (ST, runST)
import           Data.Maybe                      (fromJust)
import           Data.STRef
import           Debug.Trace                     (trace)
import           Pixelrex.Core.Array             (Ix2 (..), MMatrix, Matrix,
                                                  MonadThrow, Sz (..), Vector,
                                                  frameOfMutable, indexOfMaxBy,
                                                  loopM_, pickValues, submatrix,
                                                  submatrixOfMutable, (!), (!>))
import qualified Pixelrex.Core.Array             as A
import           Pixelrex.Core.Metric            (L1 (..))
import           Pixelrex.Core.Point
import           Pixelrex.Geometry.Core
import qualified Pixelrex.Geometry.Core          as Geom
import qualified System.Random.MWC               as MWC
import           System.Random.MWC
import           System.Random.MWC.Distributions
import           System.Random.Stateful

data GenTextureParams =
  GenTextureParams
    { _outputWidth  :: Int
    , _outputHeight :: Int
    , _genIters     :: Int
    , _neighborhood :: Int
    }

type Image = Matrix A.U (Point3D Float)

reSynthesis ::
     (PrimMonad m, MonadThrow m)
  => Gen (PrimState m)
  -> GenTextureParams
  -> Image
  -> m Image
reSynthesis gen (GenTextureParams width height iters neighborhood) sample = do
  indices <- shaffledIndices gen width height
  error "not implemented"

reSynthesisStep ::
     (PrimMonad m, MonadThrow m)
  => Gen (PrimState m)
  -> Int
  -> Int
  -> Int
  -> Image
  -> Vector A.U Ix2
  -> m (Matrix A.U Ix2)
reSynthesisStep gen w h neighborhood sample indices = do
  let n = w * h
  pickedLocs <- A.newMArray (Sz (h :. w)) (-1 :. -1)
  loopM_ 0 (< n) (+ 1) $ \i -> do
    let maxNeighbors = max 8 i
        idx@(i' :. j') = indices ! i
    neighbors' <- collectExistedNeighbors pickedLocs maxNeighbors idx
    let neighbors = A.compute neighbors'
    sublocs <-
      submatrixOfMutable
        pickedLocs
        (i' - neighborhood :. j' - neighborhood)
        (i' + neighborhood :. j' + neighborhood)
    let patch = pickValues sample sublocs
        neighbor = pickNeighbor patch neighbors sample neighborhood
    return ()
  A.freezeS pickedLocs

collectExistedNeighbors ::
     (PrimMonad m)
  => MMatrix (PrimState m) A.U Ix2
  -> Int
  -> Ix2
  -> m (Vector A.DS Ix2)
collectExistedNeighbors pickedLocs maxNeighbors (ci :. cj) = do
  collectExistedNeighbors' ci cj A.empty
  where
    Sz (h :. w) = A.sizeOfMArray pickedLocs
    collectExistedNeighbors' collected radius neighbors
      | collected == maxNeighbors = return neighbors
      | (ci + radius >= h) ||
          (ci - radius < 0) || (cj + radius >= h) || (cj - radius < 0) =
        return neighbors
      | otherwise = do
        frame <-
          frameOfMutable
            pickedLocs
            ((ci - radius) :. (cj - radius))
            ((ci + radius) :. (cj + radius))
        let found =
              A.computeAs A.U $ A.sfilter (\idx -> idx /= (-1 :. -1)) frame
            Sz sz = A.size found
            new = neighbors `A.sappend` found
        collectExistedNeighbors' (collected + sz) (radius + 1) new

pickNeighbor ::
     Matrix A.B (Maybe (Point3D Float)) -> Vector A.U Ix2 -> Image -> Int -> Ix2
pickNeighbor patch neighbors sample neighborhood = neighbors ! idx
  where
    distances =
      A.computeAs A.U $
      A.map
        (\idx@(i :. j) ->
           let patch' =
                 submatrix
                   sample
                   (i - neighborhood :. j - neighborhood)
                   (i + neighborhood :. j + neighborhood)
               distance = distanceForPatches patch patch'
            in (idx, distance))
        neighbors
    idx = indexOfMaxBy (\(_, d) -> d) distances

distanceForPatches ::
     Matrix A.B (Maybe (Point3D Float)) -> Matrix A.U (Point3D Float) -> Float
distanceForPatches patchA patchB
  | sizeA /= sizeB =
    error $ "sizes must be equal, got " ++ show sizeA ++ " " ++ show sizeB
  | otherwise = distance
  where
    sizeA = A.size patchA
    sizeB = A.size patchB
    distances =
      A.makeArrayR A.P A.Seq sizeA $ \(i :. j) ->
        case (patchA !> i ! j) of
          Just v  -> colorSpaceDistance v (patchB !> i ! j)
          Nothing -> 0
    distance = A.sum distances

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

colorSpaceDistance :: Point3D Float -> Point3D Float -> Float
colorSpaceDistance (r1, g1, b1) (r2, g2, b2) = (-1) * (log $ r * g * b)
  where
    colorsCount = 65536
    sigma = 1.0 / (20.0 * colorsCount)
    r = 1 + sigma * ((r2 - r1) ^ 2)
    g = 1 + sigma * ((g2 - g1) ^ 2)
    b = 1 + sigma * ((b2 - b1) ^ 2)
