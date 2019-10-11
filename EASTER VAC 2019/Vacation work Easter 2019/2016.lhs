Question 2

> data RTree a = Node a [RTree a] deriving Show

> exampleRTree :: RTree Int
> exampleRTree = Node 1 [Node 2 [Node 3 [], Node 4[]], Node 5 [], Node 6 [Node 7 []]]

(a)

> f :: Int -> [Int] -> Int
> f x xs = x + sum xs

> foldRTree :: (a -> [b] -> b) -> RTree a -> b
> foldRTree node (Node a ns) = node a (map (foldRTree node) ns)

(b)

> exampleRTree2 :: RTree Int
> exampleRTree2 = Node 1 [Node 2 [Node 3 [], Node 4 []], Node 5 [Node 6 [], Node 7 []]]

> depthRTree :: Int -> RTree a -> RTree Int
> depthRTree d (Node a ns) = (Node d (map (depthRTree (d+1)) ns))

> externRTree :: RTree a -> [a]
> externRTree (Node a []) = [a]
> externRTree (Node a ns) = concat (map externRTree ns)

> allEq :: [Int] -> Bool
> allEq [] = True
> allEq [n] = True
> allEq (n:ns) = (n == head ns) && allEq ns

> perfectRTree :: RTree a -> Bool
> perfectRTree = allEq.externRTree.(depthRTree 0)

(c)

> flatten :: RTree a -> [a]
> flatten (Node r rts) = r : concat (map flatten rts)

> flatten2 :: [RTree a] -> [a] -> [a]
> flatten2 rts xs = concat (map flatten rts) ++ xs

> flatten2' :: [RTree a] -> [a] -> [a]
> flatten2' [] _ = []
> flatten2' [rt] xs = flatten rt ++ xs
> flatten2' (rt:rts) xs = flatten rt ++ flatten2' rts xs

(d)

T(flatten)(h) = O (3^h)

----------------------------------------------------------------------------

Question 3

> data Zig a b = Nil | Cins a (Zag b a) deriving Show
> data Zag a b = Nal | Cans a (Zig b a) deriving Show

> exampleZig :: Zig Integer Char
> exampleZig = Cins 1 (Cans 'A' (Cins 2 Nal))

> exampleZag :: Zag String Bool
> exampleZag = Cans "C" (Cins True (Cans "D" Nil))

(a)

> headZig :: Zig a b -> a
> headZig (Cins x y) = x

> headZag :: Zag a b -> a
> headZag (Cans x y) = x

(b)

> data ZigOrZagType a b = ZigType a | ZagType b deriving Show

> lastZig :: Zig a b -> ZigOrZagType a b
> lastZig (Cins x Nal) = ZigType x
> lastZig (Cins x zag) = lastZag zag

> lastZag :: Zag a b -> ZigOrZagType b a
> lastZag (Cans x Nil) = ZagType x
> lastZag (Cans x zig) = lastZig zig

(c)

> inf :: Int
> inf = inf

The first result will be ZagType 'A' because of the pattern-matching the result will
be lastZag (Cans 'A' Nil), which is equal to 'A'.
The second result will be ZigType bottom, but only ZigType will be printed because
the bottom value cannot be calculated therefore not printed either.
The third result will be an error due to pattern-matching of the function lastZig
as we cannot use Nil instead of Nal.

(d)

> mapZig :: (a -> a) -> (b -> b) -> Zig a b -> Zig a b
> mapZig f g Nil = Nil
> mapZig f g (Cins x zag) = Cins (f x) (mapZag g f zag)

> mapZag :: (b -> b) -> (a -> a) -> Zag b a -> Zag b a
> mapZag g f Nal = Nal
> mapZag g f (Cans y zig) = Cans (g y) (mapZig f g zig)

(e)

> foldZig :: ((x -> c -> b), b) -> ((y -> b -> c), c) -> Zig x y -> b
> foldZig (fZig,eZig) (fZag,eZag) Nil = eZig
> foldZig (fZig,eZig) (fZag,eZag) (Cins x z) =
>         fZig x (foldZag (fZag,eZag) (fZig,eZig) z)

> foldZag :: ((y -> b -> c), c) -> ((x -> c -> b), b) -> Zag y x -> c
> foldZag (fZag,eZag) (fZig,eZig) Nal = eZag
> foldZag (fZag,eZag) (fZig,eZig) (Cans y z) =
>         fZag y (foldZig (fZig,eZig) (fZag,eZag) z)

This comes from analysing the pattern-matching of the 2 functions, precisely on
the type of the results of foldZig and foldZag and the functions fZig and fZag
... to explain more on paper.

--------------------------------------------------------------------

Question 4

> type Matrix a = [[a]]

(a)

> compareSize :: Matrix a -> Matrix a -> Bool
> compareSize [] [] = True
> compareSize [] _ = False
> compareSize _ [] = False
> compareSize (xs:xss) (ys:yss) = (length xs == length ys) && (n == m)
>                   where n = length xss
>                         m = length yss

(i)

> addMat :: Matrix Int -> Matrix Int -> Matrix Int
> addMat xss yss = if compareSize xss yss then [[x + y | (x,y) <- zip xs ys] | (xs,ys) <- zip xss yss]
>                                         else error "Sizes not compatible"

(ii)

> addMat' :: Matrix Int -> Matrix Int -> Matrix Int
> addMat' xss yss = if compareSize xss yss then zipWith (zipWith (+)) xss yss
>                                          else error "Sizes not compatible"

(b)

(i)

> transpose :: Matrix a -> Matrix a
> transpose [] = []
> transpose xss = if (length (head xss) > 0) then [head xs | xs <- xss] : transpose xss'
>                                            else []
>                 where xss' = [tail xs | xs <- xss]

(ii)

> transpose' :: Matrix a -> Matrix a
> transpose' xss = iter xss yss
>                  where yss = replicate (length xss) []

> iter :: Matrix a -> Matrix a -> Matrix a
> iter [] yss = yss
> iter (xs:xss) yss = iter xss (zipWith (\x ys -> ys ++ [x]) xs yss)

(c)

> multMat :: Matrix Int -> Matrix Int -> Matrix Int
> multMat [] _ = []
> multMat _ [] = []
> multMat xss yss = if length (head xss) == length yss then [[multLine (xss !! p) (yss' !! q) | q <- [0..length yss' - 1]] | p <- [0.. length xss - 1]]
>                                          else error "Sizes not compatible"
>                   where yss' = transpose yss

> multLine :: [Int] -> [Int] -> Int
> multLine xs ys = sum (zipWith (*) xs ys)

(d)

> exampleMat :: Matrix Int
> exampleMat = [[1,2,3],[4,5,6],[7,8,9]]

> exampleMat2 :: Matrix Int
> exampleMat2 = [[1,1,1],[1,1,1],[1,1,1]]

> i :: Matrix Int
> i = [[1,0,0],[0,1,0],[0,0,1]]

> powersMat :: Matrix Int -> [Matrix Int]
> powersMat m = iterate (multMat m) m

> seriesMat' :: Matrix Int -> [Matrix Int]
> seriesMat' m = scanl1 addMat (powersMat m)
