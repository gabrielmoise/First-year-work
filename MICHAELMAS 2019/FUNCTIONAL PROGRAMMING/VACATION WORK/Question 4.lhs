(a)

> type Event = String
> type Country = String
> data Medal = Gold | Silver | Bronze deriving (Eq,Show)
> type Winners = [(Event, Country, Medal)]

(b)

> countmedals :: Winners -> Medal -> Country -> Int
> countmedals winners medal country
>  | null winners = 0
>  | (medal == m) && (country == c) = 1 + countmedals (tail winners) medal country
>  | otherwise = countmedals (tail winners) medal country
>    where (e,c,m) = head winners

(c)

> score :: Winners -> Country -> Int
> score winners country = 3*countmedals winners Gold country +
>                         2*countmedals winners Silver country +
>                           countmedals winners Bronze country

(d)

> rank :: Winners -> [Country] -> [Int]
> rank winners [] = []
> rank winners (c:cs) = ranking winners c : rank winners cs

> ranking :: Winners -> Country -> Int
> ranking winners country = 1 + length (filter better winners)
>     where better (e,c,m) = score winners c > score winners country

(e)

> ljustify :: Country -> Int -> String
> ljustify country n = country ++ concat (take (n-ln) (repeat " "))
>                      where ln = length country

> rjustify :: String -> Int -> String
> rjustify number n = concat (take (n-ln) (repeat " ")) ++ number
>                     where ln = length number

> table :: Winners -> [Country] -> String
> table winners [] = (ljustify "Country" 10) ++ " Gold Silver Bronze Rank\n"
> table winners (c:cs) =
>   table winners cs ++ (ljustify c 10) ++ " " ++ (rjustify (show gold) 4) ++ " "
>             ++ (rjustify (show silver) 6) ++ " " ++ (rjustify (show bronze) 6)
>             ++ " " ++ (rjustify (show pos) 4) ++ "\n"
>   where gold = countmedals winners Gold c
>         silver = countmedals winners Silver c
>         bronze = countmedals winners Bronze c
>         pos = ranking winners c

> medaltable :: Winners -> [Country] -> IO()
> medaltable winners cs = putStr (table winners (reverse cs))
