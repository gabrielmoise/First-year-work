> module MyMaze (
>   Maze,
>   makeMaze, -- :: Size -> [Wall] -> Maze
>   hasWall,  -- :: Maze -> Place -> Direction -> Bool
>   sizeOf    -- :: Maze -> Size
> )
> where

> import Geography

> data Maze = Maze Size [Place] [Place] [Place] [Place]

Expressing the Maze type with 4 lists of places instead of one list of walls.

> makeMaze :: Size -> [Wall] -> Maze
> makeMaze (x,y) walls =
>   let  north = [(i,y-1) | i <- [0..x-1]] ++ (wallsDirection N) ++ (wallsReflected N)
>        south =  [(i,0) | i <- [0..x-1]]  ++ (wallsDirection S) ++ (wallsReflected S)
>        east  = [(x-1,j) | j <- [0..y-1]] ++ (wallsDirection E) ++ (wallsReflected E)
>        west  =  [(0,j) | j <- [0..y-1]]  ++ (wallsDirection W) ++ (wallsReflected W)
>  in Maze (x,y) north south east west
>        where wallsDirection dir = [fst wall | wall <- walls, snd wall == dir]
>              wallsReflected dir = [reflect wall (opposite dir) | wall <- walls, snd wall == (opposite dir)]

The difference from the makeMaze function from the Maze module is that now
we form the four lists of walls according to each direction. We form them by
concatenating the boundary with the walls that are in the direction we want,
that's why we use the wallsDirection function, to take all the walls that have
the second argument of the pair (recall that a wall is a pair of a Place and a
Direction) the direction we want. Additionally, the wallsReflected function gives
us all the reflected walls, which have the second argument the opposite of dir.

> reflect :: Wall -> Direction -> Place
> reflect wall dir = move dir (fst wall)

The difference here is that when we reflect something, we return the place as
we obviously know what the direction is, judging by which list we consider (that's
why we created 4 lists of places instead of 1 list of walls).

> hasWall :: Maze -> Place -> Direction -> Bool
> hasWall (Maze _ north south east west) pos d
>  | d == N = pos `elem` north
>  | d == S = pos `elem` south
>  | d == E = pos `elem` east
>  | d == W = pos `elem` west

The real optimization happens here, as when we search for a specific wall,
we start by checking in which list to search, so now we need to look into
a list which is approximately four times smaller than the initial list of
walls.

> sizeOf :: Maze -> Size
> sizeOf (Maze size _ _ _ _) = size

The sizeOf function is roughly the same, only that now we use four _ ,
instead of one, as we have four arguments (the lists of places) which, in
fact, are irrelevant to the size of the maze.
