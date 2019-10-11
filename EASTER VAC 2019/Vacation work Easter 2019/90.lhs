Question 3 - 1996

> data Tree = Tip | Bin Tree Tree

> polish :: Tree -> [Bool]
> polish Tip = [False]
> polish (Bin x y) = polish y ++ polish x ++ [True]

(a)

The size of the tree is equal to the number of leaves it has.

> size :: Tree -> Int
> size Tip = 1
> size (Bin x y) = size x + size y

In the worst-case scenario, when all the leaves are on the right, we have
T(1) = 1
T(size) = T(size-1) + T(size-1) + 1 = 2 * T(size-1) + 1
So, T(size) = 2^size - 1, or T(size) = O(2^size)

(b)

> polishCat :: [Bool] -> Tree -> [Bool]
> polishCat ys Tip = False : ys
> polishCat ys (Bin a b) = polishCat (polishCat (True : ys) a) b

> polish' :: Tree -> [Bool]
> polish' = polishCat []

(c)

> step :: [Tree] -> Bool -> [Tree]
> step xs False = Tip : xs
> step (a:b:xs) True = (Bin a b) : xs

Question 2 - 1997

(a)

> merge :: [a] -> [a] -> [a]
> merge [] ys = ys
> merge (x:xs) ys = x : merge ys xs

(b)

> mergeLists :: [[a]] -> [a]
> mergeLists = foldr merge []

> allPairs :: [a] -> [b] -> [(a,b)]
> allPairs xs ys = mergeLists [[(x,y) | y <- ys] | x <- xs]

(c)

> data BTree = Nil | Node BTree BTree deriving Show

> allTrees :: [BTree]
> allTrees = Nil : rest
>     where rest = map (uncurry Node) (allPairs allTrees allTrees)

Question 2 - 1998

> ntab :: (Int -> a) -> Int -> a
> ntab f = ntf
>          where ntf n = tabf !! n
>                tabf = [f n | n <- [0..]]

(b)

> ztab :: (Int -> a) -> Int -> a
> ztab f = ztf
>          where ztf n = if (n >= 0) then tabf !! n else ztabf !! (-n-1)
>                tabf = [f n | n <- [0..]]
>                ztabf = [f n | n <- [-1,-2..]]

(c)

> data TabTree a = Leaf a | Fork (TabTree a) (TabTree a)

> treetab f = treetf
>             where treetf n =

Question 3 - 1998

(a)

> foldr' :: (a -> b -> b) -> b -> [a] -> b
> foldr' f e [] = e
> foldr' f e (x:xs) = f x (foldr' f e xs)

(c)

> map' :: (a -> b) -> [a] -> [b]
> map' f = foldr (\x -> ((f x):)) []

> (+++) :: [a] -> [a] -> [a]
> (+++) = flip(foldr (:))

> reverse' :: [a] -> [a]
> reverse' = foldr (\x ys -> ys ++ [x]) []

(d)

> reverse'' :: [a] -> [a]
> reverse'' xs = (foldr combi id xs) []
>                 where combi x nxt = nxt.(x:)

> f :: a -> ([a] -> [a]) -> [a] -> [a]
> f x nxt = nxt.(x:)
