Question 4

(b)

> allTriples :: [(Integer,Integer,Integer)]
> allTriples = concat [triples s | s <- [0..] ]

> triples :: Integer -> [(Integer,Integer,Integer)]
> triples s = [(a,b,c) | a <- [0..s] , b <- [0..s], c <- [0..s], (a+b+c) == s]

(c)

> sieve :: [Integer] -> [Integer]
> sieve xs = filter (\x -> (x `mod` h) /= 0) xs
>            where h = head xs

> primesFunct :: [Integer] -> [Integer]
> primesFunct (x:xs) = x : primesFunct (sieve (x:xs))

> primes :: [Integer]
> primes = primesFunct [2..]

(d)

> pythagora :: [(Integer, Integer, Integer)]
> pythagora = tail [(a,b,c) | (a,b,c) <- allTriples, a*a + b*b == c*c, a < b, gcd a b == 1, gcd a c == 1, gcd b c == 1]

We use tail to get rid of the first tuple which is (0,1,1).

> pythagora2 :: [(Integer,Integer,Integer)]
> pythagora2 = tail [(min (m*m-n*n) (2*m*n),max (m*m-n*n) (2*m*n),m*m+n*n) | (m,n) <- allDoubles, m>n, gcd m n == 1, m*n `mod` 2 == 0]

> allDoubles :: [(Integer,Integer)]
> allDoubles = concat [doubles s | s <- [0..] ]

> doubles :: Integer -> [(Integer,Integer)]
> doubles s = [(a,s-a) | a <- [0..s]]
