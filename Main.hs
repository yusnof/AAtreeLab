{-
  to run:
  $ ghc -e main Main.hs < swahili-small.txt

  to compile and run:
  $ ghc -O Main.hs && ./Main < swahili-small.txt

Get-Content .\sorted-small.txt | ghc -e main Main.hs
-}

import AATree
import Data.Char

--------------------------------------------------------------------------------

main :: IO ()
main = do
  contents <- getContents
  let textList = words contents
      tree = buildTree textList
  printStatistics tree
  
buildTree :: [String] -> AATree String
buildTree = foldl (flip insert) emptyTree

printStatistics:: AATree String -> IO ()
printStatistics tree = do
  let s= size tree
  let h = height tree
  let optimalHeight= (logBase 2 (fromIntegral (s+1)))-1 -- optimal height: [log(n+1)]-1
  let optimalHeightRatio = fromIntegral h / optimalHeight
  putStrLn ("Size: "++ show s)
  putStrLn ("Height: "++show h)
  putStrLn ("Optimal height: "++ show optimalHeight)
  putStrLn ("height / optimal heigth: "++ show optimalHeightRatio)
  putStrLn (show (checkTree tree))
  putStrLn ("First 20 words: "++ first20 tree)
      


first20 tree = format (take 20 (inorder tree))
  where
    format [] = ""
    format [x] = x
    format (x:xs) = x ++" "++ format xs 
  
  


-- optimal height: [log(n+1)]-1
  -- calculate and print statistics
  -- use fromIntegral/ceiling/logBase
  --undefined

--------------------------------------------------------------------------------

