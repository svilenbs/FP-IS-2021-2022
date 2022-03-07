{--
More recursion
--}


{-- Find m-th summation of first n natural numbers.
If m > 1
  SUM(n, m) = SUM(SUM(n, m - 1), 1)
Else 
  SUM(n, 1) = Sum of first n natural numbers.
--}
{-- 
Write function calculateF
calculateF(n) = n, if n < 3
calculateF(n) = calculateF(n - 1) + 2*calculateF(n - 2) + calculateFf(n - 3), otherwise
--}
-- Write function coundDigits n a b ,calculating how many time there is the digit n in range of Integers [a,b]
-- Write function calculateSin n by the formula in week03-task.png
{--
Calculate the numbers of all n-digit strictly increasing numbers
--}
-- Write function sumOfPrimes finding the sum of all prime numbers in the interval [a, b]
-- Write function canbePrime n finding if there is a way by remove one of the digits in n the number became a prime.

