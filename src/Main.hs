import System.IO
import Data.List
import Debug.Trace

--data Colour = White | Yellow | Red | Orange | Green | Blue
data Colour = Yellow | Red | Green | Orange | Blue | White deriving (Eq)
instance Show Colour where
  show c = "\x1b[" ++ (colour c) ++ "██ " ++ "\x1b[0m"
    where
      colour Yellow = "33;1m"
      colour Red    = "31m"
      colour Green  = "32m"
      colour Orange = "33m"
      colour Blue   = "34m"
      colour White  = "37;1m"

cubeSize = 7

data CubeState = CubeState [Colour]
instance Show CubeState where
  show (CubeState c) = printCube c

-- Name (e.g. R, L'), how this maps the old to the new.
data MoveTable = MoveTable String [Int] --deriving (Show)
instance Show MoveTable where
  show (MoveTable s c) = s ++ "\n" ++ printCube c

data Axis = Xaxis | Yaxis | Zaxis deriving (Eq)
data Rotation = RotNone | RotCW | RotCCW | Rot180 deriving (Eq)

printCube :: Show a => [a] -> [Char]
printCube c =
  -- yellow
  concatMap showPaddedLine (take cubeSize rows)
  -- blue/red/green/orange rows
  ++ (concatMap (\n -> (concat $ everyCubeSize (drop n middleRows)) ++ "\n") [0..cubeSize -1])
  -- white
  ++ concatMap showPaddedLine (drop (cubeSize * 5) rows)
  where
    everyCubeSize [] = []
    everyCubeSize xs = head xs : everyCubeSize (drop cubeSize xs)

      -- rotate the rows so red lines up correctly
    middleRows = (drop (cubeSize * 3) mRows) ++ (take (cubeSize * 3) mRows)
      where mRows = map showLine (take (cubeSize * 4) (drop cubeSize rows))

    rows = splitEvery cubeSize c

    showLine l = (concatMap show l)
    showPaddedLine l = (replicate (cubeSize * 3) ' ') ++ (showLine l) ++ "\n"

-- Layout of sides:
-- Each side has pieces layed out in ascending ix from top left to bottom right.
-- |---|
-- |0 W|
-- |---|---|---|---|
-- |1 R|2 G|3 O|4 B|
-- |---|---|---|---|
-- |5 W|
-- |---|

colours = [Yellow, Red, Green, Orange, Blue, White]

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
  -- --              yellow, orange, white, red
  -- | a == Xaxis = [(0,1), (3,-1), (5,1), (1,1)] -- x: blue-green axis
  -- --             yellow, green, white, blue
  -- | a == Yaxis = [(0,2), (4,-1), (5,0), (2,1)] -- y: red-orange axis
  -- --              red, green, orange, blue
  -- | a == Zaxis = [(1,0), (2,0), (3,0), (4,0)] -- z: yellow-white axis


baseState = CubeState $ concatMap (replicate (cubeSize^2)) colours

identityMove = MoveTable "I" [0.. (cubeSize ^ 2) * 6 - 1]

-- Apply movetable to a cubestate.
makeMove :: CubeState -> MoveTable -> CubeState
makeMove (CubeState state) (MoveTable s cmap)
  = CubeState $ map (\x -> state !! x) cmap

-- 'Dot product' of two movetables. Could be wrong way around..
moveTableDot :: MoveTable -> MoveTable -> MoveTable
moveTableDot (MoveTable s1 m1) (MoveTable s2 m2)
  = MoveTable (s1 ++ s2) $ map (\x -> m1 !! x) m2

main = do
  hSetBuffering stdout NoBuffering

  putStrLn "\x1b[0m"
  print baseState

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where (first,rest) = splitAt n list


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

-- returns local face coords, ignoring which face it's on.
fromIndex :: Int -> (Int, Int)
fromIndex i = (x `mod` cubeSize, x `div` cubeSize)
  where x = i `mod` (cubeSize ^2)

toIndex :: Int -> (Int, Int) -> Int
toIndex f (x, y) = (faceToIndex f) + (cubeSize * y) + x

 -- The index where the first element of a given face is in the array.
faceToIndex f = (cubeSize ^ 2) * f

-- Convert axis and direction to a face index.
axisToFace :: Axis -> Bool -> Int
axisToFace axis isPos
  | axis == Xaxis && isPos = 4
  | axis == Xaxis && not isPos = 2
  | axis == Yaxis && isPos = 1
  | axis == Yaxis && not isPos = 3
  | axis == Zaxis && isPos = 0
  | axis == Zaxis && not isPos = 5


-- Convert a rotation to (#degrees/180)
rotToInt :: Rotation -> Int
rotToInt rot
  | rot == RotNone = 0
  | rot == RotCW = 1
  | rot == Rot180 = 2
  | rot == RotCCW = 3

-- rotates a point in facewise coordinates.
rotate rot (x, y)
  | rot == RotNone = (x, y)
  | rot == RotCW   = (y, cubeSize -x -1)
  | rot == RotCCW  = (cubeSize -y -1, x)
  | rot == Rot180  = (cubeSize -x -1, cubeSize -y -1)
