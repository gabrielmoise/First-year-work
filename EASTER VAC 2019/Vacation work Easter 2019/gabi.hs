data Tree a = Fork (Tree a) (Tree a) | Leaf a

tab :: (Int -> a) -> Int -> Int  -> (Int -> a)
tab f lo hi = ftab where ftab n = extract tree lo hi n
                         tree = make_tree f lo hi

make_tree :: (Int -> a) -> Int -> Int -> Tree a
make_tree f lo hi
    | lo == hi  = Leaf (f lo)
    | otherwise = Fork (make_tree f lo mid) (make_tree f (mid + 1) hi)
    where mid = (lo + hi) `div` 2

extract :: Tree a -> Int -> Int -> Int -> a
extract (Leaf x) _ _ _ = x
extract (Fork le ri) lo hi n
    | n <= mid  = extract le lo mid n
    | otherwise = extract ri (mid + 1) hi n
    where mid = (lo + hi) `div` 2


f x = x + 10
h = tab f 10 15

