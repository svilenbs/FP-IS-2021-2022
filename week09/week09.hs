import Data.List

-- Types..
-- data... recursive
-- deriving Show, Eq...
-- Maybe...
-- Data Tree, how to implement it. Recursive way
-- Linked list in Haskell


-- Find the maximum depth of a Tree
-- Find is there a element into the Tree
-- Traverse Tree (Inorder, Preorder, Postorder)
-- is Tree Binary Search Tree?
-- Is a Binary Tree symetric?
-- Find the sum of all elements in a tree.
-- Find sum of all elements on level i.

data Maybe1 a = Nothing1 | Just1 a

getMaybeValue Nothing1 = error "Nothing to return"
getMaybeValue (Just1 a) = a

-- Point (Int x, Iny y, String name)
data Point1 = Point1 Int Int String deriving (Show) -- similar to struct in context of C++

data Point2 = Point2 { x :: Int, y :: Int, name:: String}  deriving (Show)

data Boolean1 = False1 | True1 -- deriving (Ord) LT LE EQ...
testPoint = Point1 0 1 "A"
testPoint1 = Point2 0 1 "B"
testPoint2 = Point2 { x= 1, y = 12, name = "C"}

getPointName (Point1 _ _ name) = name
getPointX (Point1 x _ _) = x

type Points = [Point2]

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show)

testTree :: Tree a
testTree = Leaf

testTree1 = Node 1 (Node 2 (Node 4 (Node 5 Leaf Leaf) Leaf) Leaf) (Node 3 Leaf Leaf)


height Leaf = 0
height (Node _ left right) = 1 + max (height left) (height right)

-- isElem x Tree
isElem :: Eq t => t -> Tree t -> Bool
isElem _ Leaf = False
isElem x (Node a left right) = x == a || isElem x left || isElem x right

inOrder Leaf = []
inOrder (Node a left right) = inOrder left ++ [a] ++ inOrder right 

symetricTree = Node 1 (Node 2 Leaf Leaf) (Node 2 Leaf Leaf)
notSymetricTree = Node 1 (Node 2 Leaf Leaf) (Node 2 Leaf (Node 4 Leaf Leaf))
data MultypleTree a = Leaf1 | Node1 a [MultypleTree a] deriving (Show)