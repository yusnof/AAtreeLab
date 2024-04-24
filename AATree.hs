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
  deriving (Eq, Show, Read)



emptyTree :: AATree a
emptyTree = Empty 


test :: AATree Integer
test = Node 1 (Node 1 Empty 1 Empty) 2 (Node 1 Empty 3 Empty) 

test1 :: AATree Integer 
test1 = Node 3 (Node 2 Empty 2 Empty) 7 (Node 3 Empty 15 Empty)



get :: Ord a => a -> AATree a -> Maybe a
get _ Empty = Nothing
get x (Node _ l c r) 
  | x == c = Just c 
  | x > c = get x r 
  | x < c = get x l
   


-- You may find it helpful to define
split :: AATree a -> AATree a
split (Node value l c r) = error "ToDo"

--vr = value right
--vl = value left and so on..
skew  :: AATree a -> AATree a
skew (Node value (Node vr rl rc rr) center (Node vl ll lc lr)) = 
   Node  value (Node vr rl rc (Node vl ll lc lr)) center (Node value rr lc lr)  





-- and call these from insert.
insert :: Ord a => a -> AATree a -> AATree a
insert x (Node value Empty c Empty) =  Node 1 Empty x Empty
insert x (Node value left center right) 
 | x > center = Node value (insert x left) center right 
 | otherwise = helperFunction (Node value left center right) -- not sure about this one 
  where 
    helperFunction tree = skew(split tree) 


inorder :: AATree a -> [a]
inorder Empty = []
inorder (Node _ l c r) = inorder l ++ [c] ++ inorder r

size :: AATree a -> Int
size Empty = 0
size (Node _ l c r) = 1 + size l + size r 


height :: AATree a -> Int
height Empty = 0
height (Node value l c r) =  value 

--height (Node _ l c r)
--  | g > 1 + w = g 
--  | g < 1 + w = w 
--   where 
--    g,w:: Int 
--    g = 1 + height l
--    w = 1 + height r


--------------------------------------------------------------------------------
-- Optional function

remove :: Ord a => a -> AATree a -> AATree a
remove emptyTree x = x
remove x y = error "sdsd" 


  

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
isSorted (x:y:xs) = x < y && isSorted xs 

  

-- Check if the invariant is true for a single AA node
-- You may want to write this as a conjunction e.g.
--   checkLevels node =
--     leftChildOK node &&
--     rightChildOK node &&
--     rightGrandchildOK node
-- where each conjunct checks one aspect of the invariant
checkLevels :: AATree a -> Bool
checkLevels = error "checkLevels not implemented"

isEmpty :: AATree a -> Bool
isEmpty = error "isEmpty not implemented"

leftSub :: AATree a -> AATree a
leftSub = error "leftSub not implemented"

rightSub :: AATree a -> AATree a
rightSub = error "rightSub not implemented"

--------------------------------------------------------------------------------

