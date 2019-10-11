type Matrix a = [[a]]

rjustify :: Int -> String -> String
rjustify x s
  | x < (length s) = drop (length s - x) s
  | otherwise = take (x-length s) (repeat ' ') ++ s

cols :: [[a]] -> [[a]]
cols xss = foldr (\xs acc -> zipWith (:) xs acc) start xss
    where start = if (length xss == 0) then []
                       else take (maximum (map length xss)) (cycle [[]])

maxList :: Show a => [a] -> Int
maxList = maximum.map (length.show)

rjustLine :: Show a => [a] -> [String]
rjustLine xs = map (rjustify (maxList xs)) (map show xs)

table :: Show a => Matrix a -> String
table = unlines.map unwords.cols.map rjustLine.cols

-- First of all, we apply
-- map rjustLine.cols to the matrix, which means that we apply
-- rjustLine to all the rows of the transposed matrix (which are the columns
-- of the matrix, and the transpose here is cols), so we basically justify
-- them according to the biggest element (in length) from the row.
-- Then we apply cols to the matrix, to get it back to the original form,
-- then we map unwords it, so we apply unwords to every row in order to create
-- a list of Strings, one String for each row of the matrix, and then
-- we apply unlines to put a "\n" between them, so when we call
-- putStr.table of a matrix, we get the desired result printed each row
-- on a new line.
