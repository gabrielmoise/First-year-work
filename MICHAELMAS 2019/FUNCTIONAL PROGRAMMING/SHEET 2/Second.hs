--3.1
{-instance Ord a => Ord [a] where
  (<) , (<=) , (>) , (>=) :: [a] -> [a] -> Bool
  [] <= [] = True
  _ <= [] = False
  [] <= _ = True
  (x:xs) <= (y:ys) = (x < y) || ((x == y) && (xs <= ys))-}

--3.2
f :: (Num a) => a -> a
g,h :: (Num a) => a -> (a -> a)
f x = 2*x
g x y = x + y
h x y = f (g x y)

subst f g x = (f x) (g x)
fix f = f (fix f)
twice f = f . f
--selfie f = f f

{-class Ord a => Ord [a] where
	(<) , (<=) , (>) , (>=) :: [a] -> [a] -> Bool
	[] <= _ = True
	_ <= [] = False
	(x:xs) <= (y:ys) = (x < y) || ( (x==y) && (xs <= ys) )
-}
inf :: Bool
inf = not inf

const1 :: a -> (a,a)
const1 x = (x,x)

const2 :: (a,b) -> a
const2 (x,y) = x

f1,f2 :: Bool -> Bool
f1 x = True
f2 x = False

(&&&) :: Bool -> Bool -> Bool
x &&& False = False
False &&& y = False
True &&& True = True

funct :: Bool -> Bool
funct x = True

bot :: [a]
bot = bot ++ bot

asd xs = [[]] : xs
