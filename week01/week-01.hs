{--
Good stuffs to read:
http://learnyouahaskell.com/chapters
http://zvon.org/other/haskell/Outputprelude/index.html

Tasks:
https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems
--}

hello = "Hello World"

dummy x = x

-- a * x^2 + b * x + c = 0 
d a b c = b*b - 4*a*c

max1 :: Ord a => a -> a -> a
max1 a b = if a > b then a else b

-- Guards
max2 a b
    | a > b = a
    | otherwise = b

--Pattern matching
guessTheNumber 10 = "Correct one"
guessTheNumber x = "Not the correct one"

-- Why this requires "String"
max3 :: String -> String -> String
max3 a b
    | a > b = a
    | b > a = b
    | otherwise = "Equal"

fact1 0 = 1
fact1 1 = 1
fact1 x = x * fact1 (x - 1)

-- Is this readable ?
fact2 x = if x == 0 then 1 else if x == 1 then 1 else x * fact2(x-1)

fact3 x = if x == 0 || x == 1 then 1 else x * fact3(x-1)

plus a b = a + b

-- plus 4 6 == 10
-- 4 `plus` 6

-- genarate n-th fibonacci number in as much as we can different ways
fib1 0 = 0
fib1 1 = 1
fib1 x = fib1(x-1) + fib1(x-2)

-- Write function gcd1 finding greatest common divisor of 2 numbers
-- Write function lcm1 finding lowest common multiple of 2 numbers
-- Write function area finding area of a triangle by defined sides of the triangle
-- Write function biggest finding the bigest digit in a Int (Integer)