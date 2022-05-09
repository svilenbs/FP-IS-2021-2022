import Data.List
-- instance Eq for Tree, how to define it?
{-- Define for Tree Eq, Ord and Show
  1
    |-2
    | |-4
    | | |-x
    | | |-x
    | |-x
    |-3
      |-5
      | |-7
      | | |-x
      | | |-x
      | |-x
      |-6
        |-x
        |-x
The task is from https://github.com/ichko/fmi-fp-2020-21/tree/main/week-04, task 7
And all task from this week04...
--}
{-- 
Define a new data type called "Shape". Shape must have four constructors:
Circle: with one argument representing the radius;
Rectangle: with two arguments representing the width and height;
Triangle - defined by 3 sides;
Cylinder with two arguments for the radius of the base and height.
Create a shape from every type and output it.
--}

data Shape a = Circle a | Rectangle a a |  Triangle a a a | Cylinder a a deriving (Show)
-- ==, /=

instance Eq a => Eq (Shape a) where
  Circle a == Circle b = a==b
  Rectangle a b == Rectangle c d =  a == c && b == d
  Cylinder a b == Cylinder c d =  a == c && b == d
  Triangle a b c == Triangle d e f =  a == d && b == e && c == f
  _ == _ = False
  a /= b = not(a == b)
data Bool1 = True1 | False1 deriving (Eq, Ord)
instance Show Bool1 where
  show True1 = "OK"
  show False1 = "NOT OK"

-- "OK", "NOT OK"   
boolTest :: Bool1
boolTest = True1

isEqual :: Eq a => Shape a -> Shape a -> Bool
isEqual (Circle a) (Circle b) = a == b
isEqual (Rectangle a b ) (Rectangle c d) = a == c && b == d
isEqual _ _ = False

test :: Shape Integer
test = Circle 4

-- is2D = (...)?

is2D :: Shape a -> Bool
is2D (Cylinder _ _) = False
is2D _ = True
-- is2D (Rectangle _ _) = True
-- is2D Triangle {} = True
-- is2D (Circle _ ) = True
-- Define is2D object return information if the Shape is 2d object.
-- Define function area for Shape objects

-- By list of of Shapes, return the object with the biggest area.
-- By list of of Shapes return only triangles and recrangles with same area.

-- Graphs...
-- by defined graph find is there direct path between 2 elemets in graph
{-- 
Problem 80
(***) Conversions

Write predicates to convert between the different graph representations. With these predicates, all representations are equivalent; i.e. for the following problems you can always pick freely the most convenient form. The reason this problem is rated (***) is not because it's particularly difficult, but because it's a lot of work to deal with all the special cases.

Example in Haskell:

λ> graphToAdj Graph ['b','c','d','f','g','h','k'] [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')]
Adj [('b', "cf"), ('c', "bf"), ('d', ""), ('f', "bck"), ('g', "h"), ('h', "g"), ('k', "f")]
--}

-- All problems from here https://wiki.haskell.org/99_questions/80_to_89 ...

graph = [(1,[2,3]),
        (2, []),
        (3, [4]),
        (4,[]),
        (5,[])]

-- return all Nodes for this graph
getNode :: [(a, b)] -> [a]
getNode a = fst $ unzip a

getNode1 :: [(b1, b2)] -> [b1]
getNode1 a = map (\(a,_) -> a) a

isDirectPath g from to = hasPath isDirectPathHelper
  where
  hasPath Nothing = False
  hasPath (Just (_,b)) = to `elem` b
  isDirectPathHelper = find (\(a,b) -> from == a) g
