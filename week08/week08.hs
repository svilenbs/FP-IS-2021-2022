import Data.List


generate n = [ 1.. n]
generate2 n = [(x,y) | x <- [1..n], y<-[1..n], x/=y, x<y]
{--addIndex :: [(a, b, c)] -> t -> [(a, b, c, t)]
addIndex [] _ = []
addIndex ((x,y,z):xs) i =(x,y,z, i) : addIndex xs (i+1)
--

--}
-- [(1,1,1), (2,2,2)] -- > [(1,1,1,0), (2,2,2,1)]
-- [(1,1,1), (2,2,2)] -- > [((1,1,1), 0), ((2,2,2),1)] more easy to be implemented


--minDistance :: (Ord p, Floating p) => [(p, p, p)] -> p
minDistance :: [(Double,Double,Double)] -> Double
minDistance [] = error "Empty List"
minDistance points = (sort $ calculatePoints points) !! 0
    where
        calculatePoints p = [distance (fst p1) (fst p2) | p1 <- (generatePoints p), p2<- (generatePoints p), snd p1 /= snd p2, snd p1 < snd p2 ]
        generatePoints p = zip p [1..]
        distance p1@(x,y,z) p2@(x1,y1,z1) = (x-x1)*(x-x1) + (y-y1)*(y-y1) + (z-z1)*(z-z1)



f = (\x -> x+1)
g = (\x -> x+2)

isDominances f g = all (\x -> abs (g x) > abs (f x)) 

