speak xs  =(\a -> foldl (\x (y,index) -> 
    if y == a 
    then x++show (length xs - index) 
    else x++[y]) [] (zip xs [1..]) )


data Btree a = Empty | Node a (Btree a) (Btree a)
traverse1 Empty = []
traverse1 node@(Node value l r) = traverse1 l ++ helper node ++ traverse1 r
    where helper node = if isCorrect node == True then [getValue node] else [] 
getValue Empty = error "No value for Empty tree"
getValue (Node value _ _) = value

isCorrect Empty = False
isCorrect (Node _ Empty _) = False
isCorrect (Node _  _ Empty) = False
isCorrect (Node value (Node v2 _ _) (Node v3 _ _)) = value > (v2+v3)

t1 = Node 1 (Node 12 
    (Node 2 Empty Empty)
 (Node 3 Empty Empty)) (Node 20 (Node 17 (Node 13 Empty Empty) Empty) Empty)

calculate el pos = 1 + el**pos + pos**2 -- is ** OK?

seriesSum x y = sum [calculate x i | i <- [1..y] ]
seriesSum1 x y = sum $ map (\i -> calculate x i) [1..y]
