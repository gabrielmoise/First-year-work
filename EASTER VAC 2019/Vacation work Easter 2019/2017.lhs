Question 1

(c)

> pascal :: [[Integer]]
> pascal = iterate (\xs -> zipWith (+) (0:xs) (xs ++ [0])) [1]

Question 2

(a)

> merge :: [Integer] -> [Integer] -> [Integer]
> merge (x:xs) (y:ys)
>  | x == y    = x : merge xs ys
>  | x < y     = x : merge xs (y:ys)
>  | otherwise = y : merge (x:xs) ys

(b)

> hamming :: [Integer]
> hamming = h
>     where h = 1 : merge (map (2*) h) (merge (map (3*) h) (map (5*) h))

(e)

> hamming' :: [Integer] -> [Integer]
> hamming' as = h
>       where h = 1 : foldr1 merge [map (p*) h | p <- as]


Question 3

> type Grid = Matrix Int
> type Matrix a = [[a]]

(a)
(i)

> choices :: Matrix Int -> Matrix [Int]
> choices xss = [[if (x == 0) then [1..9] else [x] | x <- xs] | xs <- xss]

(ii)

> choices' :: Matrix Int -> Matrix [Int]
> choices' = map(map (\x -> if (x == 0) then [1..9] else [x]))

(b)
(i)

> cp :: [[a]] -> [[a]]
> cp [] = [[]]
> cp (xs:yss) = [x:ys | x <- xs, ys <- cp yss ]

(ii)

> cp' :: [[a]] -> [[a]]
> cp' = foldr f [[]]

> f :: [a] -> [[a]] -> [[a]]
> f xs yss = [x:ys | x <- xs, ys <- yss]

(c)

> expand :: Matrix [a] -> [Matrix a]
> expand = cp.map cp

(d)

> complete :: Grid -> Bool
> complete _ = True

> solve :: Grid -> [Grid]
> solve = (filter complete) . expand . choices

The function solve will take into consideration 9^x grids as for each
grid with a 0 cell, there are 9 possibilities and the expand function
creates all the possibilities of the grid.

(e)

> pruneRow :: [[Int]] -> [[Int]]
> pruneRow xss = map (remove (ones xss)) xss

> ones :: [[a]] -> [a]
> ones xss = [d | [d] <- xss]

> remove :: [Int] -> [Int] -> [Int]
> remove xs [d] = [d]
> remove xs ds = filter (`notElem` xs) ds

> transpose :: [[a]] -> [[a]]
> transpose [] = []
> transpose [xs] = [ [x] | x <- xs]
> transpose (xs : xss) = zipWith (:) xs (transpose xss)

> by :: Int -> [a] -> [[a]]
> by n [] = []
> by n xs = take n xs : by n (drop n xs)

> boxes :: [[a]] -> [[a]]
> boxes = map concat . concat . map transpose . by 3 . map (by 3)

> prune :: [[[Int]]] -> [[[Int]]]
> prune = pruneBy boxes . pruneBy transpose . pruneBy id
>         where pruneBy f = f . map pruneRow . f

> solveFast :: Grid -> [Grid]
> solveFast = filter complete . expand . prune . choices

Question 4

(a)

> inits :: [a] -> [[a]]
> inits [] = [[]]
> inits (x:xs) = [] : map (x:) (inits xs)

(b)

scanl :: (b -> a -> b) -> b -> [a] -> [b]

(c)

> scanl' :: (b -> a -> b) -> b -> [a] -> [b]
> scanl' f e [] = [e]
> scanl' f e (x:xs) = e : scanl' f (f e x) xs

(d)

> data Natural = Zero | Succ Natural

> foldNat :: (a -> a) -> a -> Natural -> a
> foldNat f e Zero = e
> foldNat f e (Succ m) = f (foldNat f e m)
