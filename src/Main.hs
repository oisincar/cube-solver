import System.IO
import Debug.Trace
import Data.List
import Data.Ord
import qualified Data.Vector as V

import MoveTable
-- import qualified Cube
import Cube

main = do
  hSetBuffering stdout NoBuffering

  putStrLn "\x1b[0m"
  print $ take 10 statesList
  -- let coloursLst = concat $ replicate 10000000 colours
  -- print $ length $ filter (== Yellow) coloursLst

score :: CubeState -> Int
score (CubeState c) = V.sum $ V.zipWith isEq cachedFaceColours c
  where isEq a b | a == b = 1 | otherwise = 0

cachedFaceColours = V.fromList (concatMap (replicate fArea) colours)
  where fArea = Cube.cubeSize^2

scoreRec 1 st = score st
scoreRec depth st = maximum [scoreRec (depth -1) (makeMove st m) | m <- moves]

bestMove2 st = maximumBy (comparing fst) [(scoreRec 6 (makeMove st m), m) | m <- moves]

testCube = makeMove Cube.baseState $ makeMoveSequence
  [r', u, r', d, d, r,r, u', r', u', r', d,r, u', r', d, r, r, u, l, l']

statesList :: [(Int, String, CubeState)]
statesList = (0, "Base", testCube) : map (\(_, _, st) -> let (score, (MoveTable name table)) = (bestMove2 st)
                                              in (score, name, makeMove st (MoveTable name table))
                                 ) statesList
