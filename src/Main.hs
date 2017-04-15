import System.IO
import Debug.Trace

import MoveTable
import qualified Cube

main = do
  hSetBuffering stdout NoBuffering

  putStrLn "\x1b[0m"
  print Cube.baseState
