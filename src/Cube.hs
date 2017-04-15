module Cube where

data CubeState = CubeState [Colour]
instance Show CubeState where
  show (CubeState c) = printCube c

data Axis = Xaxis | Yaxis | Zaxis deriving (Eq)

data Rotation = RotNone | RotCW | RotCCW | Rot180 deriving (Eq)

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
    splitEvery _ [] = []
    splitEvery n list = first : (splitEvery n rest)
      where (first,rest) = splitAt n list

    showLine l = (concatMap show l)
    showPaddedLine l = (replicate (cubeSize * 3) ' ') ++ (showLine l) ++ "\n"

baseState = CubeState $ concatMap (replicate (cubeSize^2)) colours
  where colours = [Yellow, Red, Green, Orange, Blue, White]

-- Returns local face coords, ignoring which face it's on.
fromIndex :: Int -> (Int, Int)
fromIndex i = (x `mod` cubeSize, x `div` cubeSize)
  where x = i `mod` (cubeSize ^2)

-- Convert face and facewise coords to an index.
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
  | rot == RotCW   = 1
  | rot == Rot180  = 2
  | rot == RotCCW  = 3

-- rotates a point in facewise coordinates.
rotate rot (x, y)
  | rot == RotNone = (x, y)
  | rot == RotCW   = (y, cubeSize -x -1)
  | rot == RotCCW  = (cubeSize -y -1, x)
  | rot == Rot180  = (cubeSize -x -1, cubeSize -y -1)
