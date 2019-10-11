data Tree a = Fork (Tree a) (Tree a) | Leaf a

tab :: (Int -> a) -> Int  -> (Int -> a)
tab f lo = ftab where ftab n = extract_inf tree lo 1 n
                      tree = make_tree_inf f lo 1

make_tree_inf :: (Int -> a) -> Int -> Int -> Tree a
make_tree_inf f lo sz = Fork lson rson
           where lson = make_tree f lo (lo + sz - 1)
                 rson = make_tree_inf f (lo + sz) (sz * 2)

extract_inf :: (Tree a) -> Int -> Int -> Int -> a
extract_inf (Fork lson rson) lo sz n 
    | n < lo + sz = extract lson lo (lo + sz - 1) n
    | otherwise   = extract_inf rson (lo + sz) (sz * 2) n

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
h = tab f 5

