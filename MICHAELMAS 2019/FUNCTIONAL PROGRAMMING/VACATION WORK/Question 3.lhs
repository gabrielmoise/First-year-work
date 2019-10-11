(a)

> interleave :: [a] -> [a] -> [a]
> interleave (x:xs) ys = x : interleave ys xs
> interleave [] ys = ys

(b)

The first 6 elements of the result of interleave [2,2] [1..] are
2,1,2,2,3,4 as we first alternate the first 2 elements of each list and then
we print the rest of the infinite list.

*Main> take 6 (interleave [2,2] [1..])
[2,1,2,2,3,4]
(0.01 secs, 74,904 bytes)

(c)

> interleaveList :: [[a]] -> [a]
> interleaveList = foldr interleave []

As the function interleave ensures us that any element from an input list
can be found in a finite number of steps, then the interleaveList function,
which is build around multiple applications of the interleave function, is
ensured to have the same property.

(d)

> bigList = map (\x -> [x,x+1]) [1,3..]

The first 8 elements of the result of interleaveList
[[1,2],[3,4],[5,6],[7,8],[9,10]...] are 1,3,2,5,4,7,6,9. Let's say that
we got to the point where we have the first two elements of the accumulator
x and y. Therefore, after interleaving it with [9,10] the first 4 elements
will be [9,x,10,y], then the first 6 elements will be [7,9,8,x,10,y] and
so on until we interleave all the lists.

*Main> take 8 (interleaveList bigList)
[1,3,2,5,4,7,6,9]
(0.01 secs, 80,424 bytes)

(e)

> allpairs :: [a] -> [b] -> [(a,b)]
> allpairs xs ys = [(x,y) | x <- xs, y <- ys]

The problem is that if we want to calculate allpairs [a,b] xs, where xs is
an infinite list. We want
every pair of the cartesian product to be found in a finite number of steps.
However, with the current definition, the pairs of the form (a,x) with x from
xs are returned first, and therefore we can't reach any pair of the form (b,x)
with x from xs in a finite number of steps, as there are infinitely many pairs
before.

(f)

> allpairs2 :: [a] -> [b] -> [(a,b)]
> allpairs2 xs ys = interleaveList [ [ (x,y) | y <- ys ] | x <- xs ]

By interleaving an infinite number of lists (finite or infinite) that
define the cartesian product of xs and ys, we are ensured to find every
pair within a finite number of steps.

(g)

> data Tree = Nil | Fork Tree Tree deriving Show

> alltrees :: [Tree]
> alltrees = Nil : rest
>    where rest = map (uncurry' Fork) (allpairs2 alltrees alltrees)

This way, we create all the finite trees of the datatype Tree. The
(uncurry Fork) function, given a pair of trees from the allpairs2 function,
(which is applied to 2 alltrees arguments, thus we create the
infinite list of trees recursively), creates the tree with the left
subtree the first element of the pair, and the right subtree the second
element of the pair.

> uncurry' :: (a -> b -> c) -> (a,b) -> c
> uncurry' f (x,y) = f x y
