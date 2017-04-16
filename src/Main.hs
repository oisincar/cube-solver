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

  print mixedCube
  print $ makeMove mixedCube d'
  -- print $ makeMove mixedCube l
  -- print $ makeMove mixedCube r'
  -- print $ makeMove mixedCube l'

score :: CubeState -> Int
score (CubeState c) = sumFaces
  where
    sumFaces = V.sum $ V.zipWith isEq cachedFaceColours c
    isEq a b | a == b = 1 | otherwise = 0

    -- sumLines = sum $ map scoreLine (splitEveryV cubeSize c)
    -- scoreLine :: V.Vector a -> Int
    -- scoreLine l = cubeSize - ((length . group . V.modify . sort) l)
    -- scoreLine l = length $ V.modify sort c

splitEveryV :: Int -> V.Vector a -> [V.Vector a]
splitEveryV n list
  | null list = []
  | otherwise = first : (splitEveryV n rest)
  where (first,rest) = V.splitAt n list

cachedFaceColours = V.fromList (concatMap (replicate fArea) colours)
  where fArea = Cube.cubeSize^2

scoreRec 1 st = score st
scoreRec depth st = maximum [scoreRec (depth -1) (makeMove st m) | m <- moves2]

bestMove2 st = maximumBy (comparing fst) [(scoreRec 3 (makeMove st m), m) | m <- moves2]

testCube = makeMove Cube.baseState $ makeMoveSequence $ moves2

statesList :: [(Int, String, CubeState)]
statesList = (0, "Base", mixedCube) :
  map (\(_, _, st) -> let (score, (MoveTable name table)) = (bestMove2 st)
                      in (score, name, makeMove st (MoveTable name table))
      ) statesList

mixedCube = makeMove Cube.baseState $ makeMoveSequence $
            map (\n -> moves2 !! (n `mod` (length moves2))) randomNums

randomNums =
  [ 9951, 1092, 5583, 3873, 2941, 7101, 9175, 7972, 5850, 4875
  , 3997, 8869, 6387, 1243, 7138, 3772, 3068, 3282, 4258, 9215
  , 1219, 7899, 8636, 9171, 5510, 9992, 576, 2809, 3447, 4591
  , 9567, 2189, 6758, 9385, 2722, 4823, 6129, 2955, 6409, 8666
  , 1109, 4733, 9848, 1100, 700, 172, 5224, 2259, 1191, 9606
  , 6049, 2774, 2903, 302, 4401, 5748, 9899, 826, 9838, 316
  , 5283, 4073, 8737, 8473, 1554, 2677, 7443, 4676, 5751, 5726
  , 874, 520, 8672, 843, 9043, 4122, 606, 8410, 673, 5867
  , 3689, 6508, 4066, 4759, 6488, 543, 7835, 5789, 7746, 4814
  , 9523, 9551, 5972, 5851, 2541, 8450, 8853, 3429, 8070, 7939
  , 3171, 6219, 5806, 6913, 7013, 8251, 2985, 1940, 6297, 605
  , 9498, 2793, 107, 7315, 2636, 3438, 6676, 9590, 8976, 1502
  , 5823, 4024, 9115, 9009, 9836, 1014, 8893, 7956, 8157, 7075
  , 9435, 3454, 2510, 1918, 5456, 4285, 6165, 2322, 8394, 4202
  , 5654, 4394, 9331, 5699, 1250, 5922, 4053, 7959, 2782, 6996
  , 4538, 8821, 3105, 8751, 4188, 4585, 9935, 7955, 3718, 866
  , 2102, 4974, 7012, 1007, 702, 9725, 5444, 8594, 3841, 7924
  , 333, 5672, 7108, 5241, 6216, 5368, 7689, 5941, 8811, 1317
  , 7203, 7819, 2763, 6961, 177, 9599, 6512, 6719, 2365, 6348
  , 5085, 4237, 5054, 7052, 742, 6295, 1896, 3395, 2351, 9881
  , 8624, 457, 4470, 7154, 4536, 5052, 6918, 1676, 6749, 1159
  , 4902, 8135, 5531, 5613, 6293, 7046, 456, 2687, 4713, 3525
  , 6788, 6968, 3450, 8211, 7418, 8754, 8766, 732, 9221, 8415
  ]
