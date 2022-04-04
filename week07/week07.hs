import Data.List

zip1 [] _ = []
zip1 _ [] = []
zip1 (x:xs) (x1:xs1) = (x,x1) : zip1 xs xs1

-- By list [a,b,c...] defining a + b*x+c*x^2... define function
-- calculate function find2 l returning 2 args lambda function returning n-th derivative in spesific point

-- find2 [1,2,3] -> 1 1 -> 8
-- 2 + 2*3
-- find2 1 + 2*x + 3*x^2 = \x y
-- 2 + 6*x
generateList l = zip l [0,1..]
nextDerivative l = map (\(x,y) -> (x*y, y-1)) l

-- repeat2 :: (Eq t1, Num t1, Num t2) => (t2 -> t2) -> t2 -> t1 -> t2 ??
-- repeat2 :: (t1 -> [a]) -> [a] -> t2 -> [a] ??
-- repeat2 :: (Eq t, Num t) => (a -> a) -> [a] -> t -> [a] ??
repeat2 :: (Eq t1, Num t1) => (t2 -> t2) -> t2 -> t1 -> t2
repeat2 f l count
    | count == 0 = l
    | otherwise = repeat2 f (f l) (count-1)
sumPoint l x= sum [a * x**b | (a,b)<-l];
find2 l = (\x y -> sumPoint(repeat2 nextDerivative (generateList [1,2,3,4,5]) x) y)

-- https://raw.githubusercontent.com/SimeonHristov99/FP_IS_Summer_21-22/main/Week_05%20-%20Lambda%20Expressions.%20Tuples/assets/forHomeTask6.png
-- https://raw.githubusercontent.com/SimeonHristov99/FP_IS_Summer_21-22/main/Week_05%20-%20Lambda%20Expressions.%20Tuples/assets/forHomeTask5.png

-- Sort from the previous time
-- qsort2 [5, 1,2,3,99,1] --> [1,1,2,3,5,9]

 -- =< 5                >5
-- [1,2,3,1] ++ [5] ++ [99]

qsort1 [] = []
qsort1 (x:xs) =  smallerSortNumbers  ++ [x] ++ biggerSortNumbers  
    where
    smallerSortNumbers = qsort1 [y | y <- xs, y <= x]
    biggerSortNumbers = qsort1 [y | y <- xs, y > x]
    

-- Modules Data.List

-- Find if one matrix is symetric. Is it possible in one line?
-- In String find the Char that is more ofen define one after another as a subset
-- TesttEst" -> 't'
-- "ValueeeeeVVV" -> 'e'

-- What is find, what is returning find... Types....

data Maybe1 a = Just1 a | Nothing