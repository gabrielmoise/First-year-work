double :: Integer -> Integer
double x = 2 * x

sum' :: (Num a) => [a]-> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

factorial :: Integer -> Integer
factorial n
    | n<0 = error "Can't use this function for negative values"
    | n==0 = 1
    | otherwise = product [1..n]

choose :: Integer -> Integer -> Integer
choose n r = div (factorial n) ((factorial r) * (factorial (n-r)))

check :: Integer -> Bool
check n = if sum [choose n r | r <- [0..n]]  == 2^n then True
                                                    else False

not1 :: Bool -> Bool
not1 True = False
not1 False = True

not2 :: Bool -> Bool
not2 x
    | x == True = False
    | otherwise = True

not3 :: Bool -> Bool
not3 x = if x==True then False else True

not4 :: Bool -> Bool
not4 x = case x of True -> False
                   False -> True
