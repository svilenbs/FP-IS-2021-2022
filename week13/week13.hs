import Data.List


poly :: [Int] -> (Int->Int)
poly l v = sum . (map (\(a,b) -> a*(v^b))) $ zip l [0,1..]


polyLambda :: [Int] -> (Int->Int)
polyLambda l v = sum . map (\(a, b) -> a*(v^b)) $ zip l [0,1..]

isEqual5 x = x == 5

isEq5 = (==) 5
isEqq5 x = 5 == x

getIndices l x =
    head [ (a,b) | (c, a) <- zip l [0,1..],
              (d, b) <- zip l [0,1..],
               c + d == x,
               a /= b
              ]

getTriangles x = [
    (a,b,c) | a <- [2..x],
              b <- [2..x],
              c <- [2..x],
              a^2 + b^2 == c^2
    ]

test = find (== 3) [1,2,3]

data Maybe1 x = Nothing1 | Just x

averageFunction l x = avg [ f x | f <- l ]

avg xs = sum xs / fromIntegral (length xs)


-- Car   (km/h)
-- Truck (km/h, Y time to start)
-- Train (km/h, Y time to stop, each T minutes)

data V = Car Int  | Truck Int Int | Train Int Int Int deriving (Show)

testV = [Car 60, Truck 90 10, Train 150  2 20]

getrequiredTime (Car speed) distance = (div distance speed)
getrequiredTime (Truck speed timeToStart) distance = (div distance speed) + timeToStart
getrequiredTime (Train speed timeStop eachMinutes) distance = (div distance speed) + (div (div distance speed) eachMinutes) * timeStop


fastertDistance testV distance = fst $ foldr1  (\(x1, y1) (x2, y2) -> if y1 > y2 then (x2, y2) else (x1, y1))
    [ (v, getrequiredTime v distance) | v <-testV ]
