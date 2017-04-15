module MoveTable where

import Data.List
import Cube

-- Name (e.g. R, L'), how this maps the old to the new.
data MoveTable = MoveTable String [Int] --deriving (Show)
instance Show MoveTable where
  show (MoveTable s c) = s ++ "\n" ++ printCube c

-- Layout of sides:
-- Each side has pieces layed out in ascending ix from top left to bottom right.
-- |---|
-- |0 W|
-- |---|---|---|---|
-- |1 R|2 G|3 O|4 B|
-- |---|---|---|---|
-- |5 W|
-- |---|

-- Tells you the direction of the 'slice' taken on each face when preforming this rotation.
-- A rotation of 0 means a slice in that axis cuts along the corrisponding axis cuts along
-- the x axis of local face coords, where y = 0.
faceRotationCache :: Axis -> [(Int, Rotation)]
faceRotationCache a
  | a == Xaxis = [(0, RotCW), (3, RotCCW), (5, RotCW), (1, RotCW)] -- x: blue-green axis
  --             yellow, green, white, blue
  | a == Yaxis = [(0, RotNone), (4, RotCCW), (5, RotNone), (2, RotCW)] -- y: red-orange axis
  --              red, green, orange, blue
  | a == Zaxis = [(1, RotNone), (2, RotNone), (3, RotNone), (4, RotNone)] -- z: yellow-white axis

identityMove = MoveTable "I" [0.. (cubeSize ^ 2) * 6 - 1]

-- 'Dot product' of two movetables. Could be wrong way around..
moveTableDot :: MoveTable -> MoveTable -> MoveTable
moveTableDot (MoveTable s1 m1) (MoveTable s2 m2)
  = MoveTable (s1 ++ s2) $ map (\x -> m1 !! x) m2


-- rightM = moveTableDot (rotateEdgeTable 0 0) (rotateFaceTable 0 1) "R"
moveTable :: Axis -> Int -> Rotation -> MoveTable
moveTable axis slice rot
  | slice == 0 =
    moveTableDot edgeTable (rotateFaceTable (axisToFace axis True) rot)
  | slice == cubeSize =
    moveTableDot edgeTable (rotateFaceTable (axisToFace axis False) rot)
  | otherwise = edgeTable
  where edgeTable = rotateEdgeTable axis slice rot

r = moveTable Xaxis 0 RotCW
f = moveTable Yaxis 0 RotCW
u = moveTable Zaxis 0 RotCW

l = moveTable Xaxis (cubeSize -1) RotCCW
b = moveTable Yaxis (cubeSize -1) RotCCW
d = moveTable Zaxis (cubeSize -1) RotCCW

  --              face ix, rot by
rotateFaceTable :: Int -> Rotation -> MoveTable
rotateFaceTable ix rot = MoveTable s
  $ take (faceToIndex ix) table
  ++ map ((toIndex ix) . (rotate rot) . fromIndex) (take (faceToIndex 1) $ drop (faceToIndex ix) table)
  ++ drop (faceToIndex (ix + 1)) table
  where (MoveTable s table) = identityMove

rotateEdgeTable :: Axis -> Int -> Rotation -> MoveTable
rotateEdgeTable axis slice rot = MoveTable "TODO" $
                            merge [0..(faceToIndex 6) -1] $ sliceRotationMap
  where
    merge (x:xs) ((i,y) : ys)
          | i > x     = x : merge xs ((i,y) : ys)
          | otherwise = y : merge xs ys
    merge xs _ = xs

    -- Map of start index to end ix after an slice rotation.
    sliceRotationMap :: [(Int, Int)]
    sliceRotationMap = sort $ take (cubeSize * 4) $ (zip vals (drop (cubeSize * (rotToInt rot)) vals))
      where vals = concat $ repeat $ concat $ valuesInSlice

    valuesInSlice = map valsEdge $ faceRotationCache axis
      where valsEdge (face, rot) = map (\(x) -> toIndex face (rotate rot (x, slice))) [0..cubeSize - 1]

-- Apply movetable to a cubestate.
makeMove :: CubeState -> MoveTable -> CubeState
makeMove (CubeState state) (MoveTable s cmap)
  = CubeState $ map (\x -> state !! x) cmap
