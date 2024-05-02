{-
  to run:
  $ ghc -e main Main.hs < swahili-small.txt

  to compile and run:
  $ ghc -O Main.hs && ./Main < swahili-small.txt
-}

import AATree
import Data.List hiding (insert)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  contents <- getContents
  xs <- (splitOn " " contents)
  foldl insert xs emptyTree
  -- split the data into words and build an AA tree
  -- use foldl
  undefined

  -- calculate and print statistics
  -- use fromIntegral/ceiling/logBase
  undefined

--------------------------------------------------------------------------------

