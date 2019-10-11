mult :: Int -> Int -> Int
mult m n = if n == 0 then 0
                     else m + mult m (n-1)
mult' :: Int -> Int -> Int
mult' m n
  | n == 0 = 0
  | (n `mod` 2) == 0 = 2 * mult' m (n `div` 2)
  | otherwise = m + 2 * mult' m ((n-1) `div` 2)

fastMult :: Int -> Int -> Int
fastMult m n = iter m n 0
  where iter m n r = if n == 0 then r else if even n then iter (m*2) (n `div` 2) r else iter m (n-1) (r+m)
        even n = n `mod` 2 == 0

raise :: Integer -> Integer -> Integer
raise m n
  | n==0 = 1
  | (n `mod`2) == 0 = raise m (n `div` 2) * raise m (n `div` 2)
  | otherwise = m * raise m ((n-1) `div` 2) * raise m ((n-1) `div` 2)

gabi :: [Int]
gabi = [x | x <-[1..600], (x `mod` 5 == 0 || x `mod` 7 == 0), x `mod` 4 /=0]
