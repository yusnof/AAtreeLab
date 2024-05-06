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
split (Node levelx lx cx (Node levely ly cy (Node levelz lz cz rz))) 
  | (levelx == levely) && (levely == levelz) = --Determine that the four node is created
  Node (levely+1) (Node levelx lx cx ly) cy (Node levelz lz cz rz)
split a = a

--rr = value right
--vl = value left and so on..
--skew  :: AATree a -> AATree a
--    x <- y    becomes   x -> y
--   /\    \             /    /\
--  A  B    C           A     B C

skew :: AATree a -> AATree a
skew Empty = Empty
skew a = a
skew (Node lvl Empty v r) = Node lvl Empty v r
skew (Node lvl (Node lvla l v rr) y r) | lvl == lvla = Node lvla l v (Node lvl rr y r)


--
insert :: Ord a => a -> AATree a -> AATree a
insert x Empty = Node 1 Empty x Empty --Base case
insert x y@(Node level left value right) --
 | x < value = helpf(Node level (insert x left) value right)
 | x > value = helpf(Node level left value (insert x right))
 | otherwise = y -- do nothing when adding the same element 
  where 
    helpf x = split(skew x) 

--Returns list of 
inorder :: AATree a -> [a]
inorder Empty = []
inorder (Node _ Empty x Empty) = [x] 
inorder (Node _ l c r) = inorder l ++ [c] ++ inorder r

-- returns the size/amount of node in a tree 
size :: AATree a -> Int
size Empty = 0
size (Node _ l c r) = 1 + size l + size r 

-- returns the number of levels / height of tree
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
-- You may want to write this as a conjunction e.g.
--   checkLevels node =
--     leftChildOK node &&
--     rightChildOK node &&
--     rightGrandchildOK node
-- where each conjunct checks one aspect of the invariant
checkLevels :: AATree a -> Bool
checkLevels Empty = True
checkLevels (Node level left _ right) = 
  level > height left &&
  level >= height right &&
  level > height (rightSub right)

isEmpty :: AATree a -> Bool
isEmpty Empty = True
isEmpty _ = False 

-- returns a left tree subtree 
leftSub :: AATree a -> AATree a
leftSub Empty           = Empty
leftSub (Node _ l c r ) = l

-- returns a right tree subtree 
rightSub :: AATree a -> AATree a
rightSub Empty           = Empty 
rightSub (Node _ l c r ) = r 

--------------------------------------------------------------------------------

