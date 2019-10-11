Question 2

> data Op = Add | Sub | Mul | Div deriving (Show,Eq)
> data Expr = Num Int | App Op Expr Expr deriving Show

> example = App Sub (App Mul (App Div (Num 4) (Num 2)) (Num 24)) (App Add (Num 4) (Num 2))

> data Elem = Operator Op | Number Int deriving Show

> type Polish = [Elem]
> type RPolish = [Elem]

> foldExpr :: (Int -> a) -> (Op -> a -> a -> a) -> Expr -> a
> foldExpr num app (Num x) = num x
> foldExpr num app (App o exp1 exp2) = app o (foldExpr num app exp1) (foldExpr num app exp2)

> convert :: Expr -> RPolish
> convert = convertCat []

> convertCat :: RPolish -> Expr -> RPolish
> convertCat ys (Num x) = (Number x) : ys
> convertCat ys (App o exp1 exp2) = convertCat (convertCat ((Operator o):ys) exp2) exp1

> evalP :: Polish -> Int
> evalP = fst . separate

> separate :: Polish -> (Int,Polish)
> separate ((Number n) : xs) = (n,xs)
> separate ((Operator op) : xs) = (apply op n m, rest)
>     where (n,elems) = separate xs
>           (m,rest) = separate elems

> apply :: Op -> Int -> Int -> Int
> apply Add = (+)
> apply Sub = (-)
> apply Mul = (*)
> apply Div = div

> evalRP :: RPolish -> Int
> evalRP = head . foldl combine []

> combine :: [Int] -> Elem -> [Int]
> combine xs (Number n) = n : xs
> combine (a:b:xs) (Operator op) = apply op b a : xs

Question 3

> type State = String
> type Rule = (Char, String)

(a)

> exampleRules :: [Rule]
> exampleRules = [('A', "BC"), ('B', "AC"), ('C', "AB")]

> applyRule :: [Rule] -> Char -> String
> applyRule rules c
>   | length nrRules == 1 = (snd.head) nrRules
>   | length nrRules == 0 = error "No rules found"
>   | otherwise = error "Too many rules found"
>       where nrRules = filter (\(x,xs) -> x == c) rules

(b)

> step :: [Rule] -> State -> State
> step rules "" = ""
> step rules (x:xs) = applyRule rules x ++ step rules xs

(c)

> runSystem :: (State, [Rule]) -> [State]
> runSystem (state, rules) = takeWhile (/= "") (iterate (step rules) state)

(d)

> isPrefix :: Eq a => [a] -> [a] -> Bool
> isPrefix [] _ = True
> isPrefix (x:xs) [] = False
> isPrefix (x:xs) (y:ys) = (x == y) && isPrefix xs ys

(e)

> findStates :: [State] -> String -> [(State,Integer)]
> findStates states str = findPrefix (index 0 states) str

> index :: Integer -> [State] -> [(State,Integer)]
> index _ [] = []
> index n (x:xs) = (x,n) : index (n+1) xs

> findPrefix :: [(State,Integer)] -> String -> [(State,Integer)]
> findPrefix [] _ = []
> findPrefix ((state,n) : states) str
>   | isPrefix str state = (state,n) : rest
>   | otherwise = rest
>       where rest = findPrefix states str
