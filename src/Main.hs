import System.IO
import Debug.Trace
import Data.List
import Data.Ord

import MoveTable
-- import qualified Cube
import Cube

main = do
  hSetBuffering stdout NoBuffering

  putStrLn "\x1b[0m"
  print $ take 10 statesList
  -- let coloursLst = concat $ replicate 10000000 colours
  -- print $ length $ filter (== Yellow) coloursLst

-- score mt | trace ("sc " ++ show mt) False = undefined
-- score (CubeState c) = sum $ map scoreFace $ zip colours (splitEvery fArea c)
--   where scoreFace (c, cs) = length $ filter (== c) cs
--         fArea = Cube.cubeSize^2
score (CubeState c) = length $ filter (==Yellow) c

scoreRec 1 st = score st
scoreRec depth st = maximum [scoreRec (depth -1) (makeMove st m) | m <- moves]

bestMove2 st = maximumBy (comparing fst) [(scoreRec 5 (makeMove st m), m) | m <- moves]

testCube = makeMove Cube.baseState $ makeMoveSequence
  [r', u, r', d, d, r,r, u', r', u', r', d,r, u', r', d, r, r, u, l, l']

statesList :: [(Int, String, CubeState)]
statesList = (0, "Base", testCube) : map (\(_, _, st) -> let (score, (MoveTable name table)) = (bestMove2 st)
                                              in (score, name, makeMove st (MoveTable name table))
                                 ) statesList
