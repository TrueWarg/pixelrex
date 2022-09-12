module Pixelrex.Geometry.Cut
  ( cutSegmentWith
  , SegmentCut(..)
  , cutPolygon
  ) where

-------------------------------------------------------------------------------------------
import           Data.List
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Maybe
import           Data.Ord
import           Data.Set               (Set)
import qualified Data.Set               as S
import           Pixelrex.Core.Algebra
import qualified Pixelrex.Core.Array    as A
import           Pixelrex.Core.Point    (Point2D)
import           Pixelrex.Geometry.Core

-------------------------------------------------------------------------------------------
type Point = Point2D Double

(-->) :: Point -> Point -> Edge
(-->) = Edge

newtype EdgeGraph =
  EdgeGraph (Map Point (Set Point))
  deriving (Eq, Ord)

data Edge =
  Edge Point Point
  deriving (Eq, Show)

data SegmentCut
  = NoCut Point Point
  | Cut Point Point Point
  deriving (Eq, Show)

data NormalizedCut
  = Entering Point
  | Exiting Point
  | Touching Point
  | AlongEdge Point Point
  deriving (Eq, Show)

data SideOfLine
  = LeftOfLine
  | DirectlyOnLine
  | RightOfLine
  deriving (Eq, Show)

-- | Nomenclature: Left/On/Right relative to scissors.
-- See also `SideOfLine`
data CutType
  = LO
  | LR
  | OL
  | OO
  | OR
  | RL
  | RO
  deriving (Eq, Show)

-------------------------------------------------------------------------------------------
instance Show EdgeGraph where
  show (EdgeGraph m) =
    unlines
      ("EdgeGraph" :
       ["    " ++ show k ++ " --> " ++ show v | (k, v) <- M.toList m])

instance Semigroup EdgeGraph where
  EdgeGraph g1 <> EdgeGraph g2 = EdgeGraph (M.unionWith (<>) g1 g2)

instance Monoid EdgeGraph where
  mempty = EdgeGraph mempty

-------------------------------------------------------------------------------------------
cutSegmentWith :: Segment -> Segment -> SegmentCut
cutSegmentWith scissors segment =
  case calculateSegmentsIntersection scissors segment of
    RealIntersection point              -> cut point
    InfinityLineIntersectionRight point -> cut point
    OnSameLine                          -> cut segmentStart
    _otherwise                          -> noCut
  where
    Segment segmentStart segmentEnd = segment
    cut point = Cut segmentStart point segmentEnd
    noCut = NoCut segmentStart segmentEnd

-------------------------------------------------------------------------------------------
cutPolygon :: Segment -> Polygon -> [Polygon]
cutPolygon scissors polygon =
  reconstructPolygons
    (polygonOrientation polygon)
    (createEdgeGraph
       scissors
       (polygonOrientation polygon)
       (map (cutSegmentWith scissors) (polygonEdges polygon)))

-------------------------------------------------------------------------------------------
createEdgeGraph :: Segment -> PolygonOrientation -> [SegmentCut] -> EdgeGraph
createEdgeGraph scissors orientation cuts =
  buildGraph (cutEdges ++ originalPolygon)
  where
    cutEdges = cutsToEdges scissors orientation cuts
    originalPolygon = polygonToEdges cuts

-------------------------------------------------------------------------------------------
buildGraph :: Foldable f => f Edge -> EdgeGraph
buildGraph =
  foldl'
    (\graph@(EdgeGraph g) (Edge start end) ->
       if start == end
         then graph
         else EdgeGraph (M.insertWith S.union start (S.singleton end) g))
    mempty

{-# INLINE buildGraph #-}
-------------------------------------------------------------------------------------------
cutsToEdges :: Segment -> PolygonOrientation -> [SegmentCut] -> [Edge]
cutsToEdges scissors orientation cuts =
  exec (cutPointsSorted scissors orientation cuts)
  where
    exec :: [NormalizedCut] -> [Edge]
    exec [] = []
    exec (Entering p:Exiting q:rest) = (p --> q) : (q --> p) : exec rest
    exec (AlongEdge _ p:Exiting q:rest) = exec (Entering p : Exiting q : rest)
    exec (Touching _:rest) = exec rest
    exec (AlongEdge p _:AlongEdge _ q:rest) = exec (AlongEdge p q : rest)
    exec (AlongEdge _ _:rest) = exec rest
    exec (Entering p:Touching q:rest) =
      exec (Entering p : Exiting q : Entering q : rest)
    exec (Entering p:AlongEdge q r:rest) =
      exec (Entering p : Exiting q : AlongEdge q r : rest)
    exec [Exiting p, Entering q] = exec [Entering p, Exiting q]
    exec bad =
      error $
      unlines
        [ "Bad portion: " ++ show bad
        , "Full list of cut lines: " ++
          show (cutPointsSorted scissors orientation cuts)
        ]

-------------------------------------------------------------------------------------------
cutPointsSorted ::
     Segment -> PolygonOrientation -> [SegmentCut] -> [NormalizedCut]
cutPointsSorted scissors orientation cuts =
  sortOn (scissorCoordinate scissors) (normalizeCuts scissors orientation cuts)

{-# INLINE cutPointsSorted #-}
-------------------------------------------------------------------------------------------
scissorCoordinate :: Segment -> NormalizedCut -> Double
scissorCoordinate scissors@(Segment scissorsStart _) normalizedCut =
  case normalizedCut of
    Entering x    -> positionAlongScissor x
    Exiting x     -> positionAlongScissor x
    Touching x    -> positionAlongScissor x
    AlongEdge x y -> min (positionAlongScissor x) (positionAlongScissor y)
  where
    positionAlongScissor p =
      dotProduct (vectorOf scissors) (vectorOf (Segment scissorsStart p))

-------------------------------------------------------------------------------------------
polygonToEdges :: [SegmentCut] -> [Edge]
polygonToEdges cuts =
  case cuts of
    Cut p x q:rest -> (p --> x) : (x --> q) : polygonToEdges rest
    NoCut p q:rest -> (p --> q) : polygonToEdges rest
    []             -> []

{-# INLINE polygonToEdges #-}
-------------------------------------------------------------------------------------------
reconstructPolygons :: PolygonOrientation -> EdgeGraph -> [Polygon]
reconstructPolygons orientation edgeGraph@(EdgeGraph graph) =
  case M.lookupMin graph of
    Nothing -> []
    Just (edgeStart, _end) ->
      case polygon of
        Polygon (_:_) -> polygon : reconstructPolygons orientation edgeGraph'
        _otherwise ->
          error $
          unlines
            [ "Empty Polygon constructed from edge graph."
            , "So edge graph cannot be deconstructed further:"
            , show edgeGraph
            ]
      where (polygon, edgeGraph') =
              minCircularConnectionPolygon orientation edgeStart edgeGraph

-------------------------------------------------------------------------------------------
-- | Extract a single polygon from an edge map by finding a minimal circular
-- connection.
minCircularConnectionPolygon ::
     PolygonOrientation -> Point -> EdgeGraph -> (Polygon, EdgeGraph)
minCircularConnectionPolygon orientation = exec Nothing S.empty
  where
    exec lastPivot visited pivot edgeGraph@(EdgeGraph edgeMap) =
      case M.lookup pivot edgeMap of
        _
          | S.member pivot visited -> (Polygon [], edgeGraph)
        Nothing -> (Polygon [], edgeGraph)
        Just toVertices ->
          case S.minView toVertices of
            Nothing -> (Polygon [], edgeGraph)
            Just (next, nothingLeft)
              | S.null nothingLeft ->
                let (Polygon rest, edgeGraph') =
                      exec
                        (Just pivot)
                        (S.insert pivot visited)
                        next
                        (EdgeGraph (M.delete pivot edgeMap))
                 in (Polygon (pivot : rest), edgeGraph')
            Just (next1, _) ->
              let useAsNext =
                    case lastPivot of
                      Nothing -> next1
                      Just from ->
                        let leftness, rightness :: Point -> Angle
                            leftness end =
                              normalizeAngle
                                (rad 0)
                                (angleOf (Segment pivot from) /-/
                                 angleOf (Segment pivot end))
                            rightness end = (-1.0) */ (leftness end)
                            pickNextVertex =
                              minimumBy $
                              comparing $
                              case orientation of
                                PolygonPos -> getRad . leftness
                                PolygonNeg -> getRad . rightness
                         in pickNextVertex (S.delete from toVertices)
                  otherVertices = S.delete useAsNext toVertices
                  (Polygon rest, edgeGraph') =
                    exec
                      (Just pivot)
                      (S.insert pivot visited)
                      useAsNext
                      (EdgeGraph (M.insert pivot otherVertices edgeMap))
               in (Polygon (pivot : rest), edgeGraph')

-------------------------------------------------------------------------------------------
normalizeCuts ::
     Segment -> PolygonOrientation -> [SegmentCut] -> [NormalizedCut]
normalizeCuts _ _ [] = []
normalizeCuts scissors orientation cutLines =
  exec (rotateToEntryPoint (mapMaybe (classifyCut scissors) cutLines))
  where
    exec :: [(Point, CutType)] -> [NormalizedCut]
    exec [] = []
    exec ((x, ty):cuts)
      | ty `elem` [LR, RL] = normalizedCutFor ty x : exec cuts
      | ty `elem` [LO, RO] = mergeCutsThroughVertex (x, ty) cuts
      | otherwise =
        error $
        unlines
          [ "Found invalid cut type " ++ show ty
          , "Maybe rotateToEntryPoint did not work as expected?"
          ]
    mergeCutsThroughVertex ::
         (Point, CutType) -> [(Point, CutType)] -> [NormalizedCut]
    mergeCutsThroughVertex (x, ty) cuts =
      case (ty, cuts) of
        (LO, (_, OR):rest) -> normalizedCutFor LR x : exec rest
        (RO, (_, OL):rest) -> normalizedCutFor RL x : exec rest
        (LO, (_, OL):rest) -> Touching x : exec rest
        (RO, (_, OR):rest) -> Touching x : exec rest
        (_, (_, OO):rest) -> followCutAlongLine x rest
        other ->
          error
            ("Encountered unexpected cut type when merging cuts through vertex: " ++
             show other)
    followCutAlongLine :: Point -> [(Point, CutType)] -> [NormalizedCut]
    followCutAlongLine x ((y, yTy):rest) =
      case yTy of
        OO -> followCutAlongLine x rest
        OL -> AlongEdge x y : exec rest
        OR -> AlongEdge x y : exec rest
        _ ->
          error
            "Tried to follow cut along line, but there is no valid option to follow."
    followCutAlongLine _ [] =
      error "Tried to follow cut along line, but there is nothing to follow"
    normalizedCutFor :: CutType -> Point -> NormalizedCut
    normalizedCutFor LR =
      case orientation of
        PolygonPos -> Entering
        PolygonNeg -> Exiting
    normalizedCutFor RL =
      case orientation of
        PolygonNeg -> Entering
        PolygonPos -> Exiting
    normalizedCutFor other =
      error $
      unlines
        [ "Can only normalize cuts that cross the line, found: " ++ show other
        , "Maybe mergeCutsThroughVertex should be applied?"
        ]
    rotateToEntryPoint [] = []
    rotateToEntryPoint (c@(_, ty):cs)
      | ty `elem` [LR, RL, LO, RO] = c : cs
      | otherwise = rotateToEntryPoint (cs ++ [c])

-------------------------------------------------------------------------------------------
classifyCut :: Segment -> SegmentCut -> Maybe (Point, CutType)
classifyCut _ NoCut {} = Nothing
classifyCut scissors (Cut l x r) =
  Just $
  case (sideOfScissors scissors l, sideOfScissors scissors r) of
    (LeftOfLine, RightOfLine) -> (x, LR)
    (RightOfLine, LeftOfLine) -> (x, RL)
    (DirectlyOnLine, DirectlyOnLine) -> (x, OO)
    (DirectlyOnLine, LeftOfLine) -> (x, OL)
    (DirectlyOnLine, RightOfLine) -> (x, OR)
    (LeftOfLine, DirectlyOnLine) -> (x, LO)
    (RightOfLine, DirectlyOnLine) -> (x, RO)
    other -> error ("Unexpected cut that cannot be classified: " ++ show other)

-------------------------------------------------------------------------------------------
sideOfScissors :: Segment -> Point -> SideOfLine
sideOfScissors scissors@(Segment scissorsStart _) p =
  let scissorsCrossPoint =
        cross2D (vectorOf scissors) (vectorOf (Segment scissorsStart p))
   in case compare scissorsCrossPoint 0 of
        LT -> RightOfLine
        EQ -> DirectlyOnLine
        GT -> LeftOfLine
