-- GABRIEL MOISE - COMPUTER SCIENCE

factor :: Integer -> (Integer,Integer)
factor n = factorFrom 2 n

factorFrom :: Integer -> Integer -> (Integer,Integer)
factorFrom m n
  | r == 0 = (m,q)
  | otherwise = factorFrom (m+1) n
  where (q,r) = n `divMod` m

{-
1.
a) factor 0 = factorFrom 2 0
   0 `divMod` 2 = (0,0), so r will be 0 and factor 0 will return (2,0)
b) First of all, factor 1 = factorFrom 2 1
   1 `divMod` 2 = (0,1), so factorFrom 2 1 returns factorFrom 3 1
   Then factorFrom 2 1 = factorFrom 3 1
   1 `divMod` 3 = (0,1), so factorFrom 3 1 returns factorFrom 4 1
   Then factorFrom 3 1 = factorFrom 4 1
   ...
   1 `divMod` i = (0,1)
   Then factorFrom i 1 = factorFrom (i+1) 1 for each i>=2
   ...
   Therefore, we will never get to a result because we will need an infinite number of recursive calls.
-}

{-
2.
    By running factor 0 we get the result (2,0).
> factor 0
(2,0)
(0.00 secs, 70,480 bytes)
    By running factor 1 the program enters an infinite loop.
> factor 1

-}

factor1 :: Integer -> (Integer,Integer)
factor1 n = factorFrom1 2 n

factorFrom1 :: Integer -> Integer -> (Integer,Integer)
factorFrom1 m n
  | r==0 = (m,q)
  | n<=m*m = (n,1)
  | otherwise = factorFrom1 (m+1) n
  where (q,r) = n `divMod` m

{-
3.
    Let's suppose that the smallest factor of n is bigger than sqrt(n) and smaller than n. Let's call it d. That would imply that there exists a number x, such that d*x=n. This way, we know that x is n/d, but because d is bigger than sqrt (n) and n=sqrt(n)*sqrt(n) we obtain that x is actually smaller than sqrt(n), but this contradicts the way we chose d, as being the smallest prime factor of n.
    Therefore the smallest factor of n must be less or equal to sqrt(n). As an observation, if there is no divisor of n between 2 and int(sqrt(n)), then n doesn't have divisors, therefore n is prime. Also, if n is prime, we will not find any number between 2 and int(sqrt(n)) that will be a divisor of him, so we can draw the conclusion that if we didn't find a divisor by searching from 2 to int(sqrt(n)), then n must be prime and we return (n,1).
    The order of the guarded operations matters because if we swap the first and second guards for example, if r==0 we might also obtain n<=m*m and the function will return (n,1) instead of the good result. So, if we called for example factor1 9 we would have obtained (9,1) instead of (3,3).
    The number of recursive calls in the worst case scenario is int(sqrt(n)), as for a prime number n we need to evaluate factorFrom1 m n for every m from 2 to int(sqrt(n))+1, case when the second guard's condition is satisfied and we stop by returning (n,1).
    To be clear, here I used int(sqrt(n)) as the smallest positive whole number whose square is less or equal to n, whereas sqrt(n) is the real value of square root of n.
-}

factor2 :: Integer -> (Integer,Integer)
factor2 n = factorFrom2 2 n

factorFrom2 :: Integer -> Integer -> (Integer,Integer)
factorFrom2 m n
  | r==0 = (m,q)
  | q<m = (n,1) --instead of n<=m*m
  | otherwise = factorFrom2 (m+1) n
  where (q,r) = n `divMod` m

{-
4.
    First of all, we could have also replaced the n<=m*m guard with n<m*m in factorFrom1 as in the case where n==m*m, which means m==sqrt(n), we first test the guard where r==0 because (q,r) = n `divMod` m and it will be True, since r = (m*m) `mod` m.
    However, this does the same number of steps as the original program, so we divide the n<m*m inequality by m and we get q<m. This is more efficient because we don't need to multiply m with itself each time and we work with smaller numbers which need less memory each time we do the verifications.
-}

factor3 :: Integer -> (Integer,Integer)
factor3 n
  | n `mod` 2 == 0 = (2,n `div` 2)
  | otherwise = factorFrom3 3 n

factorFrom3 :: Integer -> Integer -> (Integer,Integer)
factorFrom3 m n
  | r==0 = (m,q)
  | q<m = (n,1)
  | otherwise = factorFrom3 (m+2) n
  where (q,r) = n `divMod` m

{-
5.
  As we reduce the number of recursive calls by half (we eliminated the cases where the trial divisors are even and > 2), we will need at most around int(sqrt(n))/2 recursive calls, so this algorithm will be two times faster than the previous one.
-}

{-
6.
> factor3 0
(2,0)
(0.00 secs, 70,480 bytes)
> factor3 1
(1,1)
(0.00 secs, 70,448 bytes)
> factor3 17
(17,1)
(0.00 secs, 71,296 bytes)
> factor3 49
(7,7)
(0.00 secs, 75,752 bytes)
> factor3 104729
(104729,1)
(0.00 secs, 141,128 bytes)
> factor3 10102323454577
(10102323454577,1)
(1.57 secs, 648,478,472 bytes)
> factor3 123456789
(3,41152263)
(0.00 secs, 78,728 bytes)
-}

factor4 :: Integer -> (Integer,Integer)
factor4 n = factorFrom4 2 n 2

factorFrom4 :: Integer -> Integer -> Integer -> (Integer,Integer)
factorFrom4 m n s
  | r==0 = (m,q)
  | q<m = (n,1)
  | otherwise = if m == 2 then factorFrom4 3 n 2
                          else if m == 3 then factorFrom4 5 n 2 else factorFrom4 (m+s) n (6-s)
  where (q,r) = n `divMod` m

{-
7.
    The modification from factor3 is that instead of adding 2 each time (except in the case when m==2), we add 2 only when we don't get a result that is divisible by 3, so that we get rid of the multiples of 3. If we get a result that is divisible by 3 by adding 2, we skip it by adding 4 to m, instead of just 2. This swaps each time from m=5 (that's why we are treating cases m=2 and m=3 separately) as once we add 2 and we don't get a multiple of 3, the next time if we add 2 again we get a  multiple of 3, so we add 4 and then we swap again. This is slightly more efficient because the program requires less recursive calls than for factor3, as now we skip more numbers from being trial divisors.
> factor4 0
(2,0)
(0.00 secs, 74,552 bytes)
> factor4 1
(1,1)
(0.00 secs, 70,464 bytes)
> factor4 63
(3,21)
(0.00 secs, 74,552 bytes)
> factor4 1000
(2,500)
(0.02 secs, 74,472 bytes)
>factor4 10102323454577
(10102323454577,1)
(1.32 secs, 508,628,648 bytes)
-}


{-
8.
    The problem with trying to get only prime numbers as trial divisors is that we need to form a separate function that takes a list and a parameter n and returns all prime numbers from 2 to int(sqrt(n)).
    This function will call a separate function that tests if a number is prime or not and adds the number to the list only if it's prime. However, from what we worked so far, we can simply test if a number is prime or not by calling factor4 of that number and if the first element of the returned pair is our number, then it's prime.
-}

primeTo :: Integer -> [Integer]
primeTo n = [x | x <- [2..n], snd(factor4 x) == 1 ]

{-
    Now, we can create factorFrom5 which only tries prime numbers less than int(sqrt(n)). But to work with the primeTo n list, I used a "step" argument that remembers at what position in the list we are with the recursive calls. So, if we want to get the smallest prime factor we need to write factor5 = factorFrom5 2 n 0, as the first position in the primeTo n list is 0.
-}

factor5 :: Integer -> (Integer,Integer)
factor5 n = factorFrom5 2 n 0

factorFrom5 :: Integer -> Integer -> Int -> (Integer,Integer) -- step has to be Int as (!!) :: [a] -> Int -> a
factorFrom5 m n step
  | r==0 = (m,q)
  | q<m = (n,1)
  | otherwise = factorFrom5 ((primeTo n) !! (step+1)) n (step+1)
  where (q,r) = n `divMod` m

{-
    However, the problem here is that we need to form the prime number list each time we want to choose the next trial divisor, and that costs us a lot of time. Since primeTo n needs n operations to do the task, factor5 is not optimal.
    For example,
> factor5 (1009*1009)
(1009,1009)
(0.20 secs, 84,936,608 bytes)
> factor4 (1009*1009)
(1009,1009)
(0.02 secs, 84,933,864 bytes)
    We could reduce the time of making the list of prime numbers with The Sieve of Eratosthenes, but we would have still used more time to do that and lose time overall, compared to factor4
    Therefore, factor4 is more time efficient than factor5.
-}

factors :: Integer -> [Integer]
factors n = factorsFrom 2 n

factorsFrom :: Integer -> Integer -> [Integer]
factorsFrom m n
  | n==1 = []
  | otherwise = p : factorsFrom p q
  where (p,q) = factorFrom m n

{-
9.
> factors 60
[2,2,3,5]
(0.00 secs, 77,712 bytes)
> factors 100
[2,2,5,5]
(0.00 secs, 77,528 bytes)
> factors 7
[7]
(0.02 secs, 72,208 bytes)
> factors (1009*10007)
[1009,10007]
(0.02 secs, 3,677,960 bytes)
-}

factors2 :: Integer -> [Integer]
factors2 n = factorsFrom2 2 n

factorsFrom2 :: Integer -> Integer -> [Integer]
factorsFrom2 m n
  | n==1 = []
  | otherwise = p : factorsFrom2 p q
  where (p,q) = factorFrom4 m n 2
{-
> factors2 1000
[2,2,2,5,5,5]
(0.00 secs, 83,056 bytes)
> factors2 2
[2]
(0.00 secs, 66,832 bytes)
> factors2 (1009*10007)
(0.00 secs, 292,920 bytes)
-}

{-
10.
> factors 9012531452
[2,2,163,13822901]
(8.90 secs, 4,976,325,616 bytes)
> factors2 9012531452
[2,2,163,13822901]
(0.02 secs, 886,088 bytes)
> factors 12412342
[2,271,22901]
(0.03 secs, 8,320,040 bytes)
> factors2 12412342
[2,271,22901]
(0.00 secs, 135,192 bytes)
    Jevons' Problem :
> factors 8616460799
[89681,96079]
(0.06 secs, 135,192 bytes)
> factors2 8616460799
[89681,96079]
(0.05 secs, 19,449,256 bytes)
-}

{-
11.
    As this algorithm is applied for odd n, and we know that n=u*v, then u and v must also be odd. Then, x = (u+v)/2 is a whole number because (u+v) is even, and also y=(u-v)/2 is a whole number because (u-v) is even.
    I.   In the case where we found p^2-q^2-n=0 we know that p^2-q^2=n, so (p+q)*(p-q)=n, so we found two factors for n.
    II.  In the case where we found p^2-q^2-n>0, or p^2-q^2>n, as we narrowed our search to x>=p and y>=q, we know that the result respects those inequalities, so we can only continue by increasing those values in order to get closer to the result, so we need q^2 to increase, therefore q will become (q+1).
    III. In the case where we found p^2-q^2-n<0, or p^2-q^2<n, by following the same reasoning as in case II., we need to increase p this time, so p will become (p+1).
    Let's say n = c*d, then n = ((c+d)/2)^2 - ((c-d)/2)^2 and we will replace (c+d)/2 with a and (c-d)/2 with b. Therefore, n=a^2-b^2.
    We start with p = 0 and q = 0.
    Let's suppose that the algorithm never terminates.
    After each step p increases with 1 or q increases with 1. Because in both cases one of them is getting closer to the actual result, it's obvious to say that at some point (after a finite number of steps, maximum a+b-1 steps), p or q will eventually get to be equal to a or b, respectively, before the other one does. That, of course, if the program didn't terminate meanwhile, but that would mean that we are done with the reasoning as the initial statement would be false.
    Case 1. p = a
    That would mean that q < b.
    Therefore, p^2-q^2>a^2-b^2=n, so, by applying case II. q will increase by 1. As the inequality p^2-q^2>a^2-b^2 will be satisfied until q gets to be equal to b (as p is already equal to a), we will get to the point where p=a and q=b, so the program will eventually terminate.
    Case 2. q = b
    That would mean that p < a.
    Therefore, p^2-q^2<a^2-b^2=n, so, by applying case III. p will increase by 1. As the inequality p^2-q^2<a^2-b^2 will be satisfied until p gets to be equal to a (as q is already equal to b), we will get to the point where p=a and q=b, so the program will eventually terminate.
    In both cases we concluded that the program will eventually terminate so the initial statement is false.
    That means that if we narrow the problem to p>=0 and q>=0, it will always terminate (we do that to cover all cases for n, but we can start with p>=p0 and q>=q0 too when we know that p0<=a and q0<=b).
-}

{-
12.
    My initial search function for exercise 11. was :
    > search :: Integer -> Integer -> Integer -> (Integer,Integer)
    > search p q n
      | (p^2-q^2) == n = (p,q)
      | (p^2-q^2) > n = search p (q+1) n
      | otherwise = search (p+1) q n
    But then I modified it to the version where I start checking results from p>=isqrt n.
    I did that because we want to find x and y such that x^2-y^2=n in a faster way. As x^2=n+y^2, where the RHS is >= n, then also the LHS is >= n, so x^2>=n, so x>=sqrt(n), but because x is a whole number, we can start to search the result from x=isqrt(n) which is the largest integer whose square is less or equal to n.
-}

isqrt :: Integer -> Integer
isqrt = truncate . sqrt . fromInteger

search :: Integer -> Integer -> Integer -> (Integer,Integer)
search p q n
  | (p^2-q^2) == n = (p,q) -- Case I.
  | (p^2-q^2) > n = search p (q+1) n -- Case II.
  | otherwise = search (p+1) q n -- Case III.

result :: (Integer,Integer) -> [Integer]
result (p,q) = [(p+q),(p-q)]

-- This "result" function converts the x and y we found at search p q n into the list containing u and v, which are the odd divisors of n.

{-
13. Using the Fermat with from Exercise 12., which was
> fermat :: Integer -> [Integer]
> fermat n = result (search 0 0 n)

> fermat 8616460799
[96079,89681]
(0.24 secs, 235,486,888 bytes)
So, Jevons' problem is solved!
> fermat 1963272347809
[8123471,241679]
(19.60 secs, 20,316,565,240 bytes)
-}

search2 :: Integer -> Integer -> Integer -> Integer -> (Integer,Integer)
search2 p q r n
  | r == 0 = (p,q)
  | r > 0 = search2 p (q+1) (r-2*q-1) n -- As we have the new r=p^2-(q+1)^2-n, which is basically the old r - (2*q+1)
  | otherwise = search2 (p+1) q (r+2*p+1) n -- As we have the new r=(p+1)^2-q^2-n, which is basically the old r + (2*p+1)

fermat :: Integer -> [Integer]
fermat n = result (search2 (isqrt n) 0 ((isqrt n)^2 - n) n)

{-
14.
  Now, we can see and improvement from the old fermat in the examples we used at Exercise 13.:
> fermat 8616460799
[96079,89681]
(0.00 secs, 1,590,064 bytes)
> fermat 1963272347809
[8123471,241679]
(6.14 secs, 3,119,227,504 bytes)
-}

{-
  If we compute (isqrt(2^1024))^2-2^1024 we get 32317006071311007300714876688669951960444102669715484032130345427524655138867890893197201411522913463688717960921898019494119559150490921095088152386448283120630877367300996091750197750389652106796057638384067568276792218642619756161838094338476170470581645852036305042887575891541065808607552399123930385521734564076182110829912044267485667021494378337432091400804375578173293516902959351017550115541769580704823489838590463999939561383487229281910850182593813106849357791285954724448854882125685229350225459628571692166207846586010535756741612055848610098380251624748643306405371232666472431888720497281435372093440.
-}

isqrt' :: Integer -> Integer -> Integer
isqrt' n m
  | (m*m<=n) && ((m+1)*(m+1)>n) = m
  | otherwise = isqrt' n (m+1)

{-
15.
  By creating isqrt', when we run isqrt' n 0, we get the result after sqrt(n) searches, so there will be no error precision, compared to the isqrt version.
> isqrt' 1001 0
31
(0.00 secs, 88,552 bytes)
> isqrt' 15145246 0
3891
(0.02 secs, 2,093,968 bytes)
> isqrt' 1
1
(0.00 secs, 71,008 bytes)
-}

{-
  First of all, we want to prove that m^2<=n if and only if m<=sqrt(n).
  "=>" : Clain : m^2<=n => m<=sqrt(n). This comes from our initial inequality, after we apply sqrt to it (it works for all positive real numbers, therefore it works for natural numbers too)
  "<=" : m<=sqrt(n) => m^2<=n. This comes from our initial inequality, after we square it (it works for all positive real numbers, therefore it works for natural numbers too)
  So, m^2<=n if and only if m<=sqrt(n).
  Second of all, we will prove that if l<r, but l+1/=r, then l < (l+r) `div` 2 < r.
  From l<r, but l+1/=r, we can deduce that r=l+k, with k >= 2. Then, (l+r) `div` 2 = (l+l+k) `div` 2 = l + (k `div` 2). Because k>=2, we have k `div` 2 > 0, so obivously l + (k `div` 2) > l.
  Additionally, from r=l+k, we can get l=r-k, or r+l=2*r-k. Then, (l+r) `div` 2 = (r+r-k) `div` 2 = r - (k `div` 2). Because k>=2, we have k `div` 2 > 0, so obviously r - (k `div` 2) < r.
  Therefore, if l<r, but l+1/=r, then l < (l+r) `div` 2 < r.
-}

split :: (Integer,Integer) -> Integer -> (Integer,Integer)
split (l,r) n
  | ((l+r) `div` 2)^2 > n = (l,(l+r) `div` 2)
  | otherwise = ((l+r) `div` 2,r)

{-
16.
> split (1,1000) 29
(1,500)
(0.00 secs, 70,672 bytes)
> split (1,1000000) 321
(1,500000)
(0.00 secs, 74,736 bytes)
-}

binary :: (Integer,Integer) -> Integer -> Integer
binary (l,r) n
  | (l+1) == r = l
  | otherwise = binary (split (l,r) n) n

-- The binary function recursively constraints the interval (l,r) until it gets to l+1=r, which is when we stop and we return l.

isqrtNew :: Integer -> Integer
isqrtNew 0 = 0
isqrtNew 1 = 1
isqrtNew n = binary (1,n) n

{-
17.
> isqrtNew 0
0
(0.00 secs, 70,224 bytes)
> isqrtNew 124913545
11176
(0.00 secs, 105,352 bytes)
> isqrtNew 64152315264122521
253283073
(0.00 secs, 143,776 bytes)
  The isqrtNew function applied to n needs around log in base 2 of n steps, as after each step the length of the (l,r) interval becomes half of previous one. This way, if we start with (1,n), after each step we reduce the length of the interval by half this way: n -> n/2 -> n/4 ... and this terminates when we get to length 1, so after log2 of n operations.
-}

upperBound :: Integer -> Integer -> Integer
upperBound n m
  | m*m >= n = m
  | otherwise = upperBound n (2*m)

{-
18.
  For us to find the upper bound for sqrt(n) we need log2 (sqrt(n)) operations.
> upperBound 1001 1
32
(0.00 secs, 71,648 bytes)
> upperBound 231412 1
512
(0.00 secs, 72,112 bytes)
-}

isqrtNew2 :: Integer -> Integer
isqrtNew2 0 = 0
isqrtNew2 1 = 1
isqrtNew2 n = binary (1,upperBound n 1) n

{-
> isqrtNew2 124913545
11176
(0.00 secs, 90,896 bytes)
> isqrtNew2 64152315264122521
253283073
(0.00 secs, 114,720 bytes)
  For the binary search we need to make again log2 (sqrt(n)) operations to calculate the lower bound, as we already know the upper one, so l from (l,r) will be the only one that will modify after each step, because r is fixed as the upper bound.
  Therefore, in total we have log2 (sqrt(n)) + log2 (sqrt(n)) = log2 n operations, which is the same time complexity as the initial isqrtNew.
  Giving the fact that the complexity is the same, there is no improvement in speed from isqrtNew to isqrtNew2.
  However, the memory needed for isqrtNew is bigger than the one we use for isqrtNew2 as initially we worked with slightly bigger numbers, whereas for isqrtNew2 we fix the upper bound from the beginning and we dont need to go past that value, thus working with smaller values.
  In conclusion, implementing isqrtNew2 is worth the extra effort in terms of memory and giving the fact that performing operations on bigger numbers takes more time in general.
-}
