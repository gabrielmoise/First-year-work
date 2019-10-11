Question 1

> data Tree a = One' a | Two (Tree a) (Tree a) deriving Show

> exampleTree :: Tree Int
> exampleTree = Two (Two (One' 5) (Two (One' 3) (One' 2))) (One' 1)

> size :: Tree a -> Int
> size (One' _) = 1
> size (Two x y) = 1 + size x + size y

> foldTree :: (a -> b) -> (b -> b -> b) -> Tree a -> b
> foldTree one two (One' a) = one a
> foldTree one two (Two x y) = two (foldTree one two x) (foldTree one two y)

foldTree One Two = id

> size' :: Tree a -> Int
> size' = foldTree (const 1) (\x y -> 1 + x + y)

> subs :: Tree a -> [Tree a]
> subs (One' a) = [One' a]
> subs (Two x y) = (Two x y) : subs x ++ subs y

> subs' :: Tree a -> [Tree a]
> subs' = foldTree (\x -> [One' x]) (\lt rt -> (Two (head lt) (head rt)) : lt ++ rt)

> proper :: Tree a -> [Tree a]
> proper (One' _) = []
> proper (Two (One' x) y) = (One' x) : y : proper y
> proper (Two x (One' y)) = x : proper x ++ [One' y]
> proper (Two l r) = proper l ++ proper r

Question 2

> zip' :: [a] -> [b] -> [(a,b)]
> zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
> zip' _ _ = []

> zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
> zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
> zipWith' _ _ _ = []

> zip'' :: [a] -> [b] -> [(a,b)]
> zip'' = zipWith (\x y -> (x,y))

> zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
> zipWith'' f xs = map (uncurry f) . zip xs

> zero :: [Double]
> zero = 0 : zero

> one :: [Double]
> one = 1 : zero

> plus :: [Double] -> [Double] -> [Double]
> plus = zipWith (+)

> minus :: [Double] -> [Double] -> [Double]
> minus = zipWith (-)

> deriv :: [Double] -> [Double]
> deriv = zipWith (*) [0..]

> integral :: [Double] -> [Double]
> integral xs = 0 : zipWith (/) xs [1..]

> expx :: [Double]
> expx = one `plus` integral expx

> sinx :: [Double]
> sinx = integral cosx `minus` one

> cosx :: [Double]
> cosx = zero `minus` integral sinx

Question 3

> data CatList a = Nil | One a | Cat (CatList a) (CatList a) deriving Show

> rep :: [a] -> CatList a
> rep [] = Nil
> rep (x:xs) = Cat (One x) (rep xs)

> abs' :: CatList a -> [a]
> abs' = flatCat []

> flatCat :: [a] -> CatList a -> [a]
> flatCat ys Nil = ys
> flatCat ys (One x) = x : ys
> flatCat ys (Cat l r) = flatCat (flatCat ys r) l

> rev :: CatList a -> CatList a
> rev Nil = Nil
> rev (One x) = One x
> rev (Cat l r) = Cat (rev r) (rev l)

> reverse' :: [a] -> [a]
> reverse' = abs'.rev.rep

Question 4

> data Match = Match Team Int Team Int deriving Show
> type Team = String

> exampleMatch :: [[Match]]
> exampleMatch = [ [ Match "Rovers"      2  "United"   5,
>                    Match "City"        0  "Athletic" 3,
>                    Match "Corinthians" 0  "Albion"   0 ],
>                  [ Match "Rovers"      2  "Athletic" 2,
>                    Match "United"     10  "Thistle"  0,
>                    Match "Corinthians" 0  "City"     3 ] ]

> teams :: [[Match]] -> [Team]
> teams = nodups.concat.map teamsDay

> teamsDay :: [Match] -> [Team]
> teamsDay [] = []
> teamsDay ((Match t1 a t2 b) : ms) = t1 : t2 : teamsDay ms

> nodups :: Eq a => [a] -> [a]
> nodups [] = []
> nodups (x:xs) = x : nodups (filter (/= x) xs)

> data Results = Results Team [(Int,Int)] deriving Show

> extract :: [[Match]] -> [Results]
> extract mss = map (result (concat mss)) (teams mss)

> result :: [Match] -> Team -> Results
> result [] team = Results team []
> result (m:ms) team = addGoals m team (result ms team)

> addGoals :: Match -> Team -> Results -> Results
> addGoals (Match t1 a t2 b) team (Results _ listGoals)
>   | t1 == team = Results team ((a,b) : listGoals)
>   | t2 == team = Results team ((b,a) : listGoals)
>   | otherwise = Results team listGoals

> type Weight = (Int,Int,Int)

They represent in order:
- the total number of points awarded
- the goal difference
- the number of goals scored

> weight :: Results -> Weight
> weight (Results _ []) = (0,0,0)
> weight (Results team ((a,b):listGoals)) = add (a,b) (weight (Results team listGoals))

> add :: (Int,Int) -> Weight -> Weight
> add (a,b) (points,dif,scored) = (points + x, dif + a - b, scored + a)
>   where x = if (a > b) then 3 else if (a == b) then 1 else 0
