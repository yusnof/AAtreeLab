{-# OPTIONS -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

--------------------------------------------------------------------------------

module AATree (
  AATree,        -- type of AA search trees
  emptyTree,     -- AATree a
  get,           -- Ord a => a -> AATree a -> Maybe a
  insert,        -- Ord a => a -> AATree a -> AATree a
  inorder,       -- AATree a -> [a]
  remove,        -- Ord a => a -> AATree a -> AATree a
  size,          -- AATree a -> Int
  height,        -- AATree a -> Int
  checkTree      -- Ord a => AATree a -> Bool
 ) where








--------------------------------------------------------------------------------

-- AA search trees
data AATree a = Empty | Node { level :: Int,  left :: AATree a,  value :: a,  right :: AATree a }
--2 or node
  deriving (Eq, Show, Read)

emptyTree :: AATree a
emptyTree = Empty 

test22 = Empty

testTree= Node 2 (Node 1 (Node 2 Empty 1000 Empty) 2000 (Node 1 Empty 3000 Empty)) 4000 (Node 1 Empty 5000 Empty)
testSkew= skew(testTree)



test :: AATree Integer
test = Node 1 (Node 1 Empty 1 Empty) 2 (Node 1 Empty 3 Empty) 
-- insert 0 test : 

test1 :: AATree Integer 
test1 = Node 3 (Node 2 Empty 2 Empty) 7 (Node 3 Empty 15 Empty)

test3 :: AATree Integer
test3 = Node 2 (Node 1 Empty 2 Empty) 15 (Node 2 Empty 7 Empty)

--testSkew :: Bool
--testSkew | skew (test3) == Node 1 
testList :: [Integer]
testList = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,3,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100]



get :: Ord a => a -> AATree a -> Maybe a
get _ Empty = Nothing
get x (Node _ l c r) 
  | x == c = Just c 
  | x > c = get x r 
  | x < c = get x l
   

-- the four node x -> y -> z should be restructured so that y is moved up.
--              /    /    /\
-- x and z becomes the l and r  children of y (respectively). 
-- x left child is unchanged, x right child was y's left child.
-- z keeps its children.
-- in order for the pattern-match to work, split needs to be called at the leftmost node in the 4-node (x in the example above).
split :: AATree a -> AATree a
split (Node levelx lx cx (Node levely ly cy (Node levelz lz cz rz))) = Node (levely+1)
 (Node levelx lx cx ly) 
 cy
  (Node levelz lz cz rz)
split a = a

--vr = value right
--vl = value left and so on..
--skew  :: AATree a -> AATree a
--skew (Node value (Node vr rl rc rr) center (Node vl ll lc lr)) = 
--   Node value (Node vr rl rc (Node vl ll lc lr)) center (Node value rr lc lr)
--
--    x <- y    becomes   x -> y
--   /\    \             /    /\
--  A  B    C           A     B C
skew  :: AATree a -> AATree a
skew (Node levelY (Node levelX leftX valueX rightX ) valueY (Node levelC Empty valueC Empty)) = 
  Node levelX leftX valueX (Node levelY rightX valueY (Node levelC Empty valueC Empty))
skew t = t 




-- and call these from insert.
-- the 3 cases 
insert :: Ord a => a -> AATree a -> AATree a
insert x Empty = Node 1 Empty x Empty

insert x (Node level Empty value Empty) =  Node level (Node level Empty x Empty) value Empty --y@

insert x (Node level Empty value (Node l Empty v Empty)) -- Has right child but no grandchildren
  | x < value = split (skew (Node level (Node level Empty x Empty) value (Node l Empty v Empty)))  -- here we should check for skew
  | otherwise = (Node level Empty value (insert x  (Node l Empty v Empty)))

insert x (Node level left value Empty)
  | x > value = (Node level left value (Node level Empty x Empty))
  | otherwise = (Node level (insert x left) value Empty)

insert x (Node level left value right) 
 | x < value = Node level (insert x left) value right 
 | x > value = Node level left value (insert x right)
 | otherwise = error "insert: Can't insert duplicate values"
    


inorder :: AATree a -> [a]
inorder Empty = []
inorder (Node _ Empty x Empty) = [x] 
inorder (Node _ l c r) = inorder l ++ [c] ++ inorder r

size :: AATree a -> Int
size Empty = 0
size (Node _ l c r) = 1 + size l + size r 


height :: AATree a -> Int
height Empty = 0
height (Node level l c r) = level 



--------------------------------------------------------------------------------
-- Optional function

remove :: Ord a => a -> AATree a -> AATree a
remove emptyTree x = x
remove x y = error "Error in remove func" 


  
--------------------------------------------------------------------------------
-- Check that an AA tree is ordered and obeys the AA invariants

checkTree :: Ord a => AATree a -> Bool
checkTree root =
  isSorted (inorder root) &&
  all checkLevels (nodes root)
  where
    nodes x
      | isEmpty x = []
      | otherwise = x:nodes (leftSub x) ++ nodes (rightSub x)

-- True if the given list is ordered
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = x < y && isSorted xs  
  
-- Check if the invariant is true for a single AA node
-- A node's level must be greater than its left child: level(node) > level(node.left)
-- And also greater than its right-right grandchild: level(node) > level(node.right.right)
checkLevels :: AATree a -> Bool
checkLevels (Node level Empty c Empty) = level == 1 -- we need to check when rightSub and leftSub is Empty
checkLevels (Node level left c right) = level > height left && level > height (rightSub right)

 
isEmpty :: AATree a -> Bool
isEmpty Empty = True
isEmpty _ = False 


leftSub :: AATree a -> AATree a
leftSub Empty           = Empty
leftSub (Node _ l c r ) = l

rightSub :: AATree a -> AATree a
rightSub Empty           = Empty 
rightSub (Node _ l c r ) = r 

--------------------------------------------------------------------------------

