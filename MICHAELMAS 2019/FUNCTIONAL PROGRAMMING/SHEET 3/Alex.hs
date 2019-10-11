splits :: [a] -> [(a, [a])]
splits [] = []
splits (x:xs) = (x, xs) : (map (addElement x) (splits xs))
        where addElement x (a, b) = (a, x:b)

splits' :: [a] -> [(a,[a])]
splits' [] = []
splits' xs = [(xs !! p , take p xs ++ drop (p+1) xs) | p <- [0..length(xs)-1]]

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold null head tail x
    | null x = []
    | otherwise = head x : (unfold null head tail (tail x))

-----------------------------


split :: [a] -> [(a, [a])]
split xs = unfold exceed get_here get_next (0, xs)

exceed :: (Int, [a]) -> Bool
exceed (pos, xs) = pos > length xs - 1

get_here :: (Int, [a]) -> (a, [a])
get_here (pos,xs) = (xs !! pos, take pos xs ++ drop (pos + 1) xs)

get_next :: (Int, [a]) -> (Int, [a])
get_next (pos,xs) = (pos + 1, xs)

--------------------------------

split' :: [a] -> [(a, [a])]
split' xs = unfold (\(pos, xs) -> pos > (length xs) - 1) (\(pos,xs) -> (xs !! pos, (take pos xs) ++ (drop (pos + 1) xs))) (\(pos,xs) -> (pos + 1, xs)) (0, xs)
