> data Tree a = Node (Tree a) (Tree a) | Leaf a

> foldTree :: (b -> b -> b) -> (a -> b) -> Tree a -> b
> foldTree node leaf = f
>       where f (Leaf x) = leaf x
>             f (Node l r) = node (f l) (f r)

> flatten :: Tree a -> [a]
> flatten = flatCat []

> flatCat :: [a] -> Tree a -> [a]
> flatCat ys (Leaf x) = x : ys
> flatCat ys (Node ls rs) = flatCat (flatCat ys rs) ls

> flatten' :: Tree a -> [a]
> flatten' t = foldTree (.) (:) t []

> f :: a -> b
> f x = f x

> bottom :: b
> bottom = f "a"

> (!!!) :: [[a]] -> (Int,Int) -> a
> xss !!! (i,j) = (xss !! i) !! j

objects is a list of tuples of time (weight,value)

> dynamicTable :: Int -> [(Int,Int)] -> [[Int]]
> dynamicTable maxW objects = table
>   where table = [[value i j | j <- [0..length objects] ] | i <- [0..maxW]]
>         value 0 _ = 0
>         value _ 0 = 0
>         value i j
>             | fst (objects !! (j-1)) > i = table !!! (i,j-1) -- the weight too big
>             | otherwise = max (table !!! (i,j-1)) (val + table !!! (i - weight,j-1))
>                   where (weight,val) = (objects !! (j-1))

> knapsack :: Int -> [(Int,Int)] -> [(Int,Int)]
> knapsack maxW objects =
>   let weight = maxW
>       n = length objects
>       table = dynamicTable maxW objects
>       res _ 0 _ = [] -- no more objects can be added
>       res _ _ 0 = [] -- the weight is 0 so cannot add objects anymore
>       res d w j
>         | d !!! (w,j) == d !!! (w,j-1) = res d w (j-1) -- we didn't make any change here
>         | otherwise = res d (w - wgh) (j-1) ++ [objects !! (j-1)]
>               where wgh = fst (objects !! (j-1))
>   in res table weight n
