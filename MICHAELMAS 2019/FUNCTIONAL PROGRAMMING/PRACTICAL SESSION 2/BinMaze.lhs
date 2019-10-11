> module BinMaze (
>   Maze,
>   makeMaze, -- :: Size -> [Wall] -> Maze
>   hasWall,  -- :: Maze -> Place -> Direction -> Bool
>   sizeOf    -- :: Maze -> Size
> )
> where

> import Geography
> import Data.List (sort)

This is for the sort function which will be used for balancing the lists.

> data Tree a = Fork (Tree a) a (Tree a) | Empty

This is the binary search tree type, each tree being formed of a left
subtree, a node, and a right subtree, or just an Empty value.

> data Maze = Maze Size (Tree Place) (Tree Place) (Tree Place) (Tree Place)

Here, we express the Maze type with four binary search trees of places instead of
four lists of places.

> makeMaze :: Size -> [Wall] -> Maze
> makeMaze (x,y) walls =
>   let  north = [(i,y-1) | i <- [0..x-1]] ++ (wallsDirection N) ++ (wallsReflected N)
>        south =  [(i,0) | i <- [0..x-1]]  ++ (wallsDirection S) ++ (wallsReflected S)
>        east  = [(x-1,j) | j <- [0..y-1]] ++ (wallsDirection E) ++ (wallsReflected E)
>        west  =  [(0,j) | j <- [0..y-1]]  ++ (wallsDirection W) ++ (wallsReflected W)
>        treeNorth = foldl (\tree (i,j) -> insert (i,j) tree) Empty ((balance.sort) north)
>        treeSouth = foldl (\tree (i,j) -> insert (i,j) tree) Empty ((balance.sort) south)
>        treeEast  = foldl (\tree (i,j) -> insert (i,j) tree) Empty ((balance.sort) east)
>        treeWest  = foldl (\tree (i,j) -> insert (i,j) tree) Empty ((balance.sort) west)
>  in Maze (x,y) treeNorth treeSouth treeEast treeWest
>        where wallsDirection dir = [fst wall | wall <- walls, snd wall == dir]
>              wallsReflected dir = [reflect wall (opposite dir) | wall <- walls, snd wall == (opposite dir)]

The difference from the makeMaze function from MyMaze.lhs is that now we also
create the tree versions of the four lists(north,south,east,west) by "inserting"
each Place element from the lists into the corresponding trees. We do that with
a foldl so that we start by "inserting" the first element of the list and then
continuing with the rest of the list. The lists are first sorted with the
standard sort function imported from Data.List and then they are balanced
as we'll see below.

> reflect :: Wall -> Direction -> Place
> reflect wall dir = move dir (fst wall)

This function is the same as the one from MyMaze.lhs

> balance :: (Ord a, Eq a) => [a] -> [a]
> balance xs
>  | length xs == 0 = []
>  | length xs == 1 = xs
>  | length xs `mod` 2 == 1 = [xs !! (half)] ++ (balance (take half xs)) ++ (balance (drop (half + 1) xs))
>  | otherwise = [xs !! (half-1)] ++ (balance (take (half - 1) xs)) ++ (balance (drop half xs))
>       where half = length xs `div` 2

Here, we take a sorted list and we return a balanced sorted list, which, when
"inserted" in the binary search tree, will reduce the height of the tree as much
as possible, so that we don't have to go too deep to find an element (Place).
The idea is that we begin with the middle element from the list, and then we
continue with the balanced list consisting of the elements before it and then
with the balanced list consisting of the elements after it. I took separate
cases when the length of the list is odd and when it is even.
For example,

*BinMaze> balance [1..10]
[5,2,1,3,4,8,6,7,9,10]

*BinMaze> balance [1..20]
[10,5,2,1,3,4,7,6,8,9,15,12,11,13,14,18,16,17,19,20]

> insert :: Ord a => a -> Tree a -> Tree a
> insert x Empty = Fork Empty x Empty
> insert x (Fork left a right)
>    | x < a = Fork (insert x left) a right
>    | otherwise = Fork left a (insert x right)

This is the function that "inserts" the value according to where it should be.
The idea is that if we have to "insert" x into an Empty tree, we simply put
x in the node, and if we have (x < a) we "insert" it in the left subtree,
otherwise we insert it in the right subtree (notice that "insert" is a
recursive function).

> hasWall :: Maze -> Place -> Direction -> Bool
> hasWall (Maze _ treeNorth treeSouth treeEast treeWest) pos d
>  | d == N = pos `search` treeNorth
>  | d == S = pos `search` treeSouth
>  | d == E = pos `search` treeEast
>  | d == W = pos `search` treeWest

The only difference here is that we now need a search function as we
now have to check if the element is in a binary search tree or not, compared
to the case when we used the "elem" function in MyMaze.lhs to check if an
element is in a list or not.

> search :: (Ord a,Eq a) => a -> Tree a -> Bool
> search x Empty = False
> search x (Fork left a right)
>  | x == a = True
>  | x < a = search x left
>  | otherwise = search x right

This function returns False if we get to check if an element is in the
Empty binary search tree or not, and it returns True if we found it in a
node. If it is smaller than the current node, we search in the
left subtree, otherwise we search in the right subtree, recursively.

> sizeOf :: Maze -> Size
> sizeOf (Maze size _ _ _ _) = size

This function is exactly the same as in MyMaze.lhs, as we similarly do
not care what the trees are in order to determine the size of the maze.
