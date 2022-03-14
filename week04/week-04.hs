{--
List, almost everything around lists
Partially higher order functions
--}

-- [index]
-- Write function length1
length1 :: Num p => [a] -> p
length1 [] = 0
length1 xs = 1 + (length1 (tail xs))

length2 [] = 0
length2 (_:xs) = 1 + length2 xs


-- Write function elem1
elem1 :: Eq t => t -> [t] -> Bool
elem1 _ [] = False
elem1 p (x:xs)
    | p == x = True
    | otherwise = elem1 p xs

-- Write function reverse1
-- reverse1 [1,2,3] -> [3,2,1]
reverse1 xs = reverHelper xs []
    where
    reverHelper xs1 xs2
        | length xs1 == 0 = xs2
        | otherwise = reverHelper (tail xs1) ([head xs1] ++ xs2)

rev1 [] = []
rev1 (x:xs) = rev1 xs ++ [x]
-- Write function take1
-- Write function drop1
-- Write function getLastElement returning the last element in a list
getLastElement xs = (reverse1 xs) !! 0 

getLastElement1 [] = error "Empty list"
getLastElement1 (x:[]) = x
getLastElement1 (x:xs) = getLastElement xs

-- Write function maximum1 returning the biggest element in a list 
-- Write function getLastButOneElement returning the element before the last
-- getLastButOneElement [1,2,3,42,1,2] -> 1
getLastButOneElement :: [a] -> a
getLastButOneElement [] = error "Empty list"
getLastButOneElement (x:[]) = error "List must have at least 2 elements" -- [x]
getLastButOneElement (x:[s]) = x -- x:_:[]
getLastButOneElement (x:xs) = getLastButOneElement xs

-- Write function union1 for two lists
-- Write function sumInt on an Integer list. In case of empty list, result should be 1.
-- sumInt [1,2,3] == 6
sumInt xs = sumIntHelper xs 0
    where
    sumIntHelper (x:xs) sum1
        | length xs == 0 = sum1+x
        | otherwise = sumIntHelper xs (sum1+x)

sumInt1 :: Num p => [p] -> p
sumInt1 [] = 0
sumInt1 (x:xs) = x + sumInt1 xs

-- Write function multiple on an Integer list.
-- Write function avg1 returning the average element (sum/length) of a list
-- Write function zip1 working with list and returning list of 2 elements list. Make the same with tuple. 
-- Write function primesList returning list of all primes numbers


isPrime 1 = False
isPrime 2 = True
isPrime x = isPrimeHelper x 2 (x-1) -- can we optimise this validation?
    where
        isPrimeHelper x from to -- not neccesery so send x as a param since he is in the same scope
            | from == to = True
            | mod x from == 0 = False
            | otherwise  = isPrimeHelper x (from+1) to

primesList [] = []
primesList (x:xs)
    | isPrime x == True = x: primesList xs
    | otherwise = primesList xs

isPrimeGeneric :: (a -> Bool) -> [a] -> [a] -- filter
isPrimeGeneric _ [] = []
isPrimeGeneric function (x:xs)
    | function x == True = x: isPrimeGeneric function xs
    | otherwise = isPrimeGeneric function xs

sum3 :: Num a => a -> a-> a

sum3 a b = a+b

sum4 :: Num a => a -> a
sum4 x = sum3 19 x

-- Write function isAnyPrimes [Int] -> Bool retuning True if there is a prime number in the list, False otherwise

isAnyPrimes [] = False
isAnyPrimes (x:xs)
    | isPrime x == True = True 
    | otherwise = isAnyPrimes xs


-- (**) Drop every N'th element from a list. Problem 16
-- Rotate a list N places to the left. Problem 19
-- Run-length encoding of a list Problem 10
-- encoding1 "aaasdsss" == [(3,'a'), (1,'s'), (1,'d'), (3,'s')]
-- Write function that generate all possible tuple lists (Integer, Integer, Integer) for a^2 + b^2 = c^2 for up to number c
-- Based on the [(String, Number)] represented for name of a city and temperature, return the name of the city with the highest temperature from the list.