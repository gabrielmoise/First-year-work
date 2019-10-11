--9.1

data Nat = Zero | Succ Nat deriving (Show,Eq,Ord)

int :: Nat -> Int
int Zero = 0
int (Succ x) = 1 + int x

nat :: Int -> Nat
nat 0 = Zero
nat x = Succ (nat (x-1))

add :: Nat -> Nat -> Nat
add x Zero = x
add x (Succ y) = Succ (add x y)

mul :: Nat -> Nat -> Nat
mul _ Zero = Zero
mul x (Succ y) = add x (mul x y)

pow :: Nat -> Nat -> Nat
pow _ Zero = Succ Zero
pow x (Succ y) = mul x (pow x y)

tet :: Nat -> Nat -> Nat
tet _ Zero = Succ Zero
tet x (Succ y) = pow x (tet x y)

--9.2

foldNat :: (a -> a) -> a -> Nat -> a
foldNat f e Zero = e
foldNat f e (Succ x) = f (foldNat f e x)

unfoldNat :: (t -> Bool) -> (Nat -> Nat) -> (t -> t) -> t -> Nat
unfoldNat done first next x
  | done x = Zero
  | otherwise = first (unfoldNat done first next (next x))

isZero :: Nat -> Bool
isZero Zero = True
isZero _ = False

isSucc :: Nat -> Bool
isSucc = not.isZero

zero :: Nat -> Nat
zero Zero = Zero

succ' :: Nat -> Nat
succ' (Succ x) = x

int' :: Nat -> Int
int' = foldNat (+1) 0

nat' :: Int -> Nat
nat' = unfoldNat (==0) Succ pred

add' :: Nat -> Nat -> Nat
add' x = foldNat Succ x

mul' :: Nat -> Nat -> Nat
mul' x = foldNat (add' x) Zero

pow' :: Nat -> Nat -> Nat
pow' x = foldNat (mul' x) (Succ Zero)

tet' :: Nat -> Nat -> Nat
tet' x = foldNat (pow' x) (Succ Zero)

-- EXERCISE 10.4

data Liste a = Snoc (Liste a) a | Lin deriving (Show,Eq)

cat :: Liste a -> Liste a -> Liste a
cat xs Lin = xs
cat xs (Snoc ys y) = Snoc (cat xs ys) y

folde :: (b -> a -> b) -> b -> Liste a -> b
folde f lin Lin = lin
folde f lin (Snoc xs x) = f (folde f lin xs) x

-- id=folde Snoc Lin as folde is a foldr and Snoc acts
-- as a function for foldl, Snoc :: Liste a -> a -> Liste a,
-- so we flip to obtain id

cat' :: Liste a -> Liste a -> Liste a
cat' xs = folde Snoc xs

list :: Liste a -> [a]
list = folde (\xs x -> xs ++ [x]) []

fold :: (a -> b -> b) -> b -> [a] -> b
fold cons nil [] = nil
fold cons nil (x:xs) = cons x (fold cons nil xs)

liste :: [a] -> Liste a
liste = fold (\x xs -> cat' (Snoc Lin x) xs) Lin

-- In our case, cons is the lambda function which, when given
-- and element x and an already formed Liste xs, concatenate the
-- Liste which only has x (Snoc Lin x) with the liste xs, so that at the end
-- we have the Liste result in a correct order (order of the elements
-- from the original list), so the list [a,b,c,d,...] will be
-- (Snoc Lin a) `cat'` (Snoc Lin b) `cat'` (Snoc Lin c) ...

-- If we have an infinite list, lets say [1..], then liste will
-- have to calculate (Snoc Lin 1) `cat'` liste [2..], then
-- (Snoc Lin 1) `cat'` (Snoc Lin 2) `cat'` liste [3..]  and so on and will
-- never reach a result.

-- The infinite objects of type Liste a are the limits of
-- the chains of partial lists, for example Snoc bottom;Snoc(Snoc bottom)a;
-- Snoc(Snoc(Snoc bottom)a)b ...

tailfold :: (b -> a -> b) -> b -> [a] -> b
tailfold cons nil [] = nil
tailfold cons nil (x:xs) = tailfold cons (cons nil x) xs

tailfolde :: (a -> b -> b) -> b -> Liste a -> b
tailfolde cons nil Lin = nil
tailfolde cons nil (Snoc xs x) = cons x (tailfolde cons nil xs)

-- id = tailfolde (flip Snoc) Lin, as we do not need to flip Snoc this time

list' :: Liste a -> [a]
list' = tailfolde (\x xs -> xs ++ [x]) []

-- Similar to list, but with the lambda function flipped, to respect
-- the type of tailfolde(which is a foldr for Liste)

liste' :: [a] -> Liste a
liste' = tailfold Snoc Lin

-- EXERCISE 10.5

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold n h t x
  | n x = []
  | otherwise = h x : unfold n h t (t x)

-- id = unfold nul head tail

unfolde :: (b -> Bool) -> (b -> b) -> (b -> a) -> b -> Liste a
unfolde n h t x
  | n x = Lin
  | otherwise = Snoc (unfolde n h t (h x)) (t x)

-- id = unfolde (==Lin) (\Snoc xs x -> xs) (\Snoc xs x -> x)

inite :: Liste a -> a
inite (Snoc Lin x) = x
inite (Snoc xs x) = inite xs

laste :: Liste a -> Liste a
laste (Snoc Lin x) = Lin
laste (Snoc xs x) = Snoc (laste xs) x

list'' :: Eq a => Liste a -> [a]
list'' = unfold (==Lin) inite laste

liste'' :: [a] -> Liste a
liste'' = unfolde null init last

filter' :: (a -> Bool) -> [a] -> [a]
filter' p  = fold h []
  where h x = if p x then ([x]++) else ([]++)
