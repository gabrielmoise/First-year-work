Question 3

(a)

> unfold' :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]

(b)

> unfold' p f g = map f . takeWhile (\ x -> p x == False) . iterate g

(c)

> data RTree a = Node a [RTree a] deriving Show

> foldRTree :: (a -> [b] -> b) -> RTree a -> b
> foldRTree node (Node a ns) = node a (map (foldRTree node) ns)

(d)

> exampleRTree = Node 1 [Node 2 [Node 3 [], Node 4 []], Node 5 [], Node 6 [Node 7 []]]

(i)

> dft :: RTree a -> [a]
> dft (Node a []) = [a]
> dft (Node a ns) = a : concat(map dft ns)

(ii)

> dft' :: RTree a -> [a]
> dft' = foldRTree (\ x xss -> x : concat xss)

(iii)

> unfoldRTree :: ([RTree a] -> Bool) -> (a -> b) -> ([RTree a] -> [RTree a]) -> RTree a -> [b]
> unfoldRTree p f g (Node a rts) = if (p rts) then [f a] else (f a) : concat (map (unfoldRTree p f g) (g rts))

> dft'' :: RTree a -> [a]
> dft'' = unfoldRTree null id id
