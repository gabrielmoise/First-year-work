knapsack :: Int -> [(Int, Int)] -> [(Int, Int)]
knapsack w xs = snd $ dp w xs ((0, []):(repeat (minBound, [])))

dp :: Int -> [(Int, Int)] -> [(Int, [(Int, Int)])] -> (Int, [(Int, Int)])
dp w [] prev = list_max $ take (w + 1) prev
dp w ((weight, value):xs) prev = dp w xs (zipWith value_max prev new)
    where new = (take weight (repeat (minBound, []))) ++ (map add_item prev)
          add_item (x, xs) = (x + value, (weight, value):xs)

value_max :: (Int, [(Int,Int)]) -> (Int, [(Int,Int)]) -> (Int, [(Int,Int)])
value_max (x, xs) (y, ys) = if (x > y) then (x, xs) else (y, ys)

list_max :: [(Int, [(Int, Int)])] -> (Int, [(Int,Int)])
list_max = foldr value_max (minBound, [])
