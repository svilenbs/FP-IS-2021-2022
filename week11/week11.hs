-- Lets talk about Trees again...
import Data.List;
data Maybe1 a = Nothing1 | Just1 a 

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show, Eq)
--- B 4 -> 0
-- /   \ 
--Y 1   Z 7 -> 1
--      / \
--      6  9 -> 2


testTree :: Tree Integer
testTree = Node 4 (Node 1 Leaf Leaf) (Node 7 (Node 6 Leaf Leaf) (Node 10 Leaf Leaf))
-- Show Eq Ord

-- function sumTree return sum of all elements in a Tree
-- sumTree testTree == 27

{-- 
sumTree :: Tree a -> a
sumTree Nil = 0
sumTree (Node value left right) = value + sumTree left + sumTree right
--}

sumTree Leaf = 0
sumTree (Node key l r) = key + sumTree l + sumTree r

-- Confirm if a Tree is BST. In case to be True this statement B>=Y and Z>B should be true for each Tree/subtree
--- B 
-- / \
--Y  Z

-- Find sum of all elements on level i in a Tree
-- sumOnLevel 2 testTree = 15
-- sumOnLevel k (Node value l r) = sumHelper k 0

sumOnLevel _ Leaf = 0
sumOnLevel 0 (Node value _ _) = value
sumOnLevel k (Node value l r) = sumOnLevel (k-1) l + sumOnLevel (k-1) r


testTreeChar = Node 'a' (Node 'b' (Node 'f' Leaf Leaf) Leaf) (Node 'c' (Node 'd' Leaf Leaf) (Node 'e' Leaf Leaf))
t = Node 'a' (Node 'b' Leaf Leaf) Leaf

containsWordHelper :: Eq a => [a] -> Tree a -> Bool
containsWordHelper [] Leaf = True 
containsWordHelper _ Leaf = False


-- containsWordHelper word (Node key l r)
--     | length word == 0 =  not (l /= Leaf || r /= Leaf)
-- --  | length word == 0 && l == Leaf && r == Leaf = True	
--     | head word /= key = False  
-- --  | head word == key && (length word == 1) && l == Leaf && r == Leaf = True
-- --	| head word == key && (length word > 1) && l == Leaf && r == Leaf = False
-- 	| otherwise = containsWordHelper (tail word) l || containsWordHelper (tail word) r
-- testNode (Node key l r) word =  head word == key && (l /= Leaf || r /= Leaf)


containsWord Leaf [] = True
containsWord Leaf _ = False 
containsWord (Node value left right) word
    | length word == 1 && (left /= Leaf || right /= Leaf) = False
    | length word == 1 && head word == value && left == Leaf && right == Leaf = True
    | otherwise = head word == value && (containsWord left (tail word) || containsWord right (tail word))

-- Check if we have a number in a Tree that is meet more than once in the Tree?

-- Graph....
-- Problem 81
-- (**) Path from one node to another one
-- Write a function that, given two nodes a and b in a graph, returns all the acyclic paths from a to b.
