module Cube where

import qualified Data.Vector as V

data CubeState = CubeState (V.Vector Colour)
instance Show CubeState where
  show (CubeState c) = printCube $ V.toList c

data Axis = Xaxis | Yaxis | Zaxis deriving (Eq)

data Rotation = RotNone | RotCW | RotCCW | Rot180 deriving (Eq)

data Colour = Yellow | Red | Green | Orange | Blue | White deriving (Eq)
instance Show Colour where
  -- show c = "\x1b[" ++ (colour c) ++ " ██" ++ "\x1b[0m"
  show c = "\x1b[" ++ (colour c) ++ "██ "
    where
      colour Yellow = "1;33m"
      colour Red    = "0;31m"
      colour Green  = "0;32m"
      colour Orange = "0;33m"
      colour Blue   = "0;34m"
      colour White  = "1;37m"

cubeSize = 7

printCube :: Show a => [a] -> [Char]
printCube c =
  "\n"
  -- yellow
  ++ concatMap showPaddedLine (take cubeSize rows)
  -- blue/red/green/orange rows
  ++ (concatMap (\n -> (concat $ everyCubeSize (drop n middleRows)) ++ "\n") [0..cubeSize -1])
  -- white
  ++ concatMap showPaddedLine (drop (cubeSize * 5) rows)
  -- reset ansii keys
  ++ "\x1b[0m"
  where

    everyCubeSize [] = []
    everyCubeSize xs = head xs : everyCubeSize (drop cubeSize xs)

    -- rotate the rows so red lines up correctly
    middleRows = (drop (cubeSize * 3) mRows) ++ (take (cubeSize * 3) mRows)
      where mRows = map showLine (take (cubeSize * 4) (drop cubeSize rows))

    rows = splitEvery cubeSize c

    showLine l = (concatMap show l)
    showPaddedLine l = (replicate (cubeSize * 3) ' ') ++ (showLine l) ++ "\n"

baseState = CubeState $ V.fromList $ concatMap (replicate (cubeSize^2)) colours
colours = [Yellow, Red, Green, Orange, Blue, White]
coloursStr x = ["u", "f", "r", "b", "l", "d"] !! x

splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where (first,rest) = splitAt n list

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
  | axis == Xaxis && isPos = 2
  | axis == Xaxis && not isPos = 4
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
rotate :: Rotation -> (Int, Int) -> (Int, Int)
rotate rot (x, y)
  | rot == RotNone = (x, y)
  | rot == RotCW   = (y, cubeSize -x -1)
  | rot == RotCCW  = (cubeSize -y -1, x)
  | rot == Rot180  = (cubeSize -x -1, cubeSize -y -1)
