> import Geography
> import BinMaze

======================================================================

Draw a maze.

***************************************
*              Question 2             *
* Complete the definition of drawMaze *
***************************************

> drawMaze :: Maze -> IO()
> drawMaze maze = putStr((unlines.reverse)(draw maze 0))

This function is going to print the result we obtain from the function draw,
a list of strings formed by walls and spaces. We apply unlines because we
want a "\n" between each row. The use of the "reverse" function will be
explained below.

> draw :: Maze -> Int -> [String]
> draw maze index
>  | index < y = (horizontal maze index 0) : (vertical maze index 0) : draw maze (index+1)
>  | index == y = [concat (take x (repeat "+--") ++ ["+"])] -- the northerly boundary
>         where (x,y) = sizeOf maze

First, we notice that there are (y+1) horizontal walls (with '+' and '-') and
y vertical walls (with |). Therefore, we created y pairs of walls, the last
case (when we get to the last wall) returning the last orizontal row (which
is the northerly boundary). The "index" argument knows at what pair of walls we
are at each moment, so it knows the line where we are in the maze (however
we use the coordinates in the opposite order to the normal way we build the
list of strings, so that's why we need a "reverse" the drawMaze function).
The functions horizontal and vertical, given the maze, the line where we are
and the position (the column of the maze, which is 0 at the beginning) create
the String which needs to be formed according to the list of walls, the
checking being done with the predefined function hasWall.

> horizontal :: Maze -> Int -> Int -> String
> horizontal maze index position
>  | (position < x) && (hasWall maze (position,index) S) = "+--" ++ horizontal maze index (position+1)
>  | (position < x) && (hasWall maze (position,index) S == False) = "+  " ++ horizontal maze index (position+1)
>  | otherwise = "+"
>         where (x,y) = sizeOf maze

Here, we observe that we need a '+' after each position, and at the end we also
have a '+'. So, we check if the current position has a wall in its South
direction (as that's how we create the list of walls, looking always down), if
there is a wall, we write "+--", if not we write "+  ", and at the end we put
a "+" to finish the horizontal row of walls. We also end the recursive calls when
the position reaches x, as that's where the function needs to stop.

> vertical :: Maze -> Int -> Int -> String
> vertical maze index position
>  | (position < x) && (hasWall maze (position,index) W) = "|  " ++ vertical maze index (position+1)
>  | (position < x) && (hasWall maze (position,index) W == False) = "   " ++ vertical maze index (position+1)
>  | otherwise = "|"
>         where (x,y) = sizeOf maze

Similarly, here in the case of having a wall to the West (that's how we build the
vertical rows) we put a "|  ", if not we put "   "(3 spaces) and at the end
we simply put a "|".

*Main> drawMaze largeMaze
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|                       |                 |  |     |     |           |
+  +--+--+--+--+--+--+  +  +  +--+--+--+  +  +  +  +  +  +  +--+--+--+
|                    |  |  |  |           |  |  |  |  |  |  |     |  |
+--+--+--+--+--+--+  +  +  +  +  +--+--+--+  +  +  +  +  +  +  +  +  +
|                    |  |  |  |  |           |  |  |  |     |  |  |  |
+  +--+--+--+--+--+--+  +  +  +  +  +--+--+  +  +  +  +--+--+  +  +  +
|        |           |  |  |  |  |  |        |  |  |           |  |  |
+--+--+  +  +--+--+--+  +--+  +  +  +  +--+--+--+  +--+--+--+  +  +  +
|     |  |           |        |     |  |        |     |        |  |  |
+  +  +  +--+--+--+  +--+--+--+--+--+  +  +  +  +--+  +  +--+--+  +  +
|  |  |     |     |  |                 |  |  |  |     |        |  |  |
+  +  +--+  +  +  +  +  +--+--+--+--+--+  +  +  +  +--+--+--+  +  +  +
|  |        |  |  |  |           |     |  |  |  |     |        |     |
+  +--+--+--+  +  +  +--+--+--+  +  +--+  +  +  +--+  +  +--+--+--+--+
|              |  |  |           |  |  |  |  |        |              |
+--+--+--+--+--+  +  +  +--+--+--+  +  +  +  +--+--+--+--+--+--+--+  +
|                 |     |           |  |  |                       |  |
+  +--+--+--+--+--+--+  +  +--+--+--+  +  +--+--+--+--+--+--+--+  +  +
|           |        |  |           |  |        |                 |  |
+--+--+--+  +--+--+  +  +--+--+--+  +  +--+--+  +  +--+--+--+--+--+  +
|           |     |  |     |     |  |  |     |  |                    |
+  +--+--+--+  +  +  +  +  +  +  +  +  +  +  +  +--+--+--+--+--+--+--+
|           |  |  |  |  |  |  |  |  |  |  |  |                       |
+--+--+--+  +  +  +  +  +  +  +  +  +  +  +  +  +--+--+--+--+--+--+  +
|           |  |  |  |  |  |  |  |  |  |  |     |     |     |     |  |
+  +--+--+--+  +  +  +  +  +  +  +  +  +  +--+--+--+  +  +  +  +  +  +
|  |     |     |     |  |     |  |  |  |           |  |  |  |  |     |
+  +  +  +  +  +--+--+--+--+--+  +  +  +--+--+--+  +  +  +  +  +--+--+
|  |  |  |  |        |           |  |           |  |     |     |     |
+  +  +  +  +--+--+  +  +--+--+--+  +--+--+--+  +  +--+--+--+--+  +  +
|  |  |  |  |     |  |           |     |     |  |  |              |  |
+  +  +  +  +  +  +  +--+--+--+  +  +  +  +  +  +  +  +--+--+--+--+  +
|  |  |     |  |  |  |           |  |  |  |  |  |        |     |     |
+  +  +--+  +  +  +  +  +--+--+--+--+  +  +  +  +--+--+  +  +  +  +--+
|  |     |  |  |  |  |              |  |  |  |           |  |  |     |
+  +--+  +  +  +  +--+--+--+--+--+  +  +  +  +--+--+--+  +  +  +--+  +
|     |  |  |  |           |     |  |     |              |  |        |
+  +  +  +  +  +--+--+--+  +  +  +  +--+--+--+--+--+--+--+  +--+--+--+
|  |  |  |  |  |           |  |  |                       |           |
+  +  +  +  +  +  +--+--+--+  +  +--+--+--+--+--+--+--+  +--+--+  +  +
|  |  |  |  |  |           |  |                       |           |  |
+  +  +  +  +--+--+--+--+  +  +--+--+--+--+--+--+--+--+--+--+--+--+  +
|  |     |                 |                                         |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
(0.15 secs, 8,268,232 bytes)

*Main> drawMaze smallMaze
+--+--+--+--+
|     |  |  |
+  +--+  +  +
|        |  |
+--+  +  +  +
|     |     |
+--+--+--+--+
(0.00 secs, 224,704 bytes)

*Main> drawMaze impossibleMaze
+--+--+--+
|     |  |
+  +  +--+
|  |     |
+  +--+  +
|        |
+--+--+--+
(0.00 secs, 188,872 bytes)

======================================================================

Solve the maze, giving a result of type:

> type Path = [Direction]

***************************************
*            Questions 3--4           *
*     Complete the definition of      *
*              solveMaze              *
***************************************

Question 3
----------

> solveMaze :: Maze -> Place -> Place -> Path
> solveMaze maze (x1,y1) (x2,y2) = solveMazeIter maze (x2,y2) [((x1,y1),[])]

The solveMaze function gets the result from solveMazeIter, which starts with
the list of places and paths from ((x1,y1),[]).

> solveMazeIter :: Maze -> Place -> [(Place, Path)] -> Path
> solveMazeIter maze (x2,y2) list
>  | (fst.head) list == (x2,y2) = (snd.head) list
>  | otherwise = solveMazeIter maze (x2,y2) (drop 1 (makeList maze (x2,y2) list))

Here, we always check first if we got to the desired result, and in that case
we just return the path we have up to that place and if not, we add at the end
of the list of paths the possible ways to continue, and then we drop the first
element of the list, as we already checked it and used it to add the new paths
to follow. Therefore, at each step, we are only interested in the head of the
list.

> makeList :: Maze -> Place -> [(Place,Path)] -> [(Place,Path)]
> makeList maze (x2,y2) list = list ++ (path N) ++ (path S) ++ (path W) ++ (path E)
>          where path = check maze (x2,y2) list

The makeList function, given the list, adds to it the possible paths from
the head of the list (that's the one we are currently interested in), where the
function "path" creates the pathing for each direction with the help of the
function "check" below. So, we add the four paths in the four possible
directions, only if there is no wall in that direction.

> check :: Maze -> Place -> [(Place,Path)] -> Direction -> [(Place,Path)]
> check maze (x2,y2) list dir =
>       if not(hasWall maze start dir)
>               then [(move dir start, pathing ++ [dir])]
>               else []
>   where (start,pathing) = ((fst.head) list,(snd.head) list)

This function verifies if there is a path in that direction i.e. if there
is no wall ahead, if it is, it returns [], and if we have the possibility to
continue in that direction it returns the place obtained by "moving"
in that direction and the path to that place, which is the old path plus
the direction in which we moved. The start value is the Place we currently are,
and the pathing is the way we got there from the initial starting point.

*Main> solveMaze smallMaze (0,0) (3,2)
[E,N,E,S,E,N,N]
(0.02 secs, 626,896 bytes)

*Main> solveMaze (makeMaze (4,4) []) (0,0) (3,3)
[N,N,N,E,E,E]
(0.00 secs, 2,163,336 bytes)
This was for a 4x4 empty maze.

*Main> solveMaze (makeMaze (5,5) []) (0,0) (4,4)
[N,N,N,N,E,E,E,E]
(0.17 secs, 343,836,768 bytes)
This was for a 5x5 empty maze. We observe that we need more time in this case
and 150 times more memory.

For the next example, with a 6x6 the program runs for a very long time.
Also, for the largeMaze we still need a very long time to get to a result,
as there are too many operations to be made.

Additionally, for the impossible maze we receive no result, as we did not
treat it as a separate case. The next approach to the problem will.

*Main> solveMaze impossibleMaze (0,0) (2,2)

======================================================================

Question 4
----------

> fastSolveMaze :: Maze -> Place -> Place -> Path
> fastSolveMaze maze (x1,y1) (x2,y2) =
>        fastSolveMazeIter maze (x2,y2) [((x1,y1),[])] []

The differecence from the solveMaze function is that we use the
fastSolveMazeIter function that has an additional argument, which is
called visited, which initially is [], as we have not visited anything
so far. You might argue that we obviously have visited the start place,
but in my algorithm, the visited list only updates after we have "proccessed"
the place we currently are i.e. after we consider all the possible pathings from
that place and add them to the list. So, now we start with the empty list.

> fastSolveMazeIter :: Maze -> Place -> [(Place, Path)] -> [Place] -> Path
> fastSolveMazeIter maze (x2,y2) list visited
>  | list == [] = error "Impossible pathing"
>  | start == (x2,y2) = pathing
>  | not (((fst.head) list) `elem` visited) =
>    fastSolveMazeIter maze (x2,y2) (drop 1 (fastMakeList maze (x2,y2) list visited)) (visited ++ [start])
>  | otherwise = fastSolveMazeIter maze (x2,y2) (drop 1 list) visited
>        where (start,pathing) = ((fst.head) list,(snd.head) list)

First of all, we notice that in the list argument we have the places that
we still need to consider to find a way to get to the target location. We first
have the starting place, which is the only one we have to consider, and after
that, we update the list constantly after each place, by adding the possible
ways of continuing the pathing and by dropping the considered element, which
is the head of the list. We notice that, with this new idea of having a visited
list of places we already found a path to, we are able to eventually get to an
empty list of places to consider. That happens when we visited every place we
could, and as we didn't return a result (we get a result when we reach the target)
that means that we cannot get to the destination. So, we first check if the list
is empty, as we see that the next guards we use head on the list argument, so
we must first check if it is non-empty. In the case when the list is empty, we
error the "Impossible pathing" message. Now, the check that we got to the target
is the same as for solveMazeIter. The next guard checks if the head of the list i.e.
the place that we want to "proccess" has been visited or not: if it hasn't been
visited, we update the list with the fastMakeList below, and we also update the
visited argument saying we already got to that place. Otherwise, we simply drop
the element from the list and we go on. The where instance is the same as for the
check function, so as to make the code clearer.

> fastMakeList :: Maze -> Place -> [(Place,Path)] -> [Place] -> [(Place,Path)]
> fastMakeList maze (x2,y2) list visited =
>      list ++ [(move dir start, pathing ++ [dir]) | dir <- [N,S,E,W], not (hasWall maze start dir)]
>           where (start,pathing) = ((fst.head) list,(snd.head) list)

There is not difference here from the makeList function, just the fact that
we now use a single function that performs the check too. We don't need to
check that the paths we go to are visited or not as we will drop them anyways
in the fastSolveMazeIter function in the "otherwise" guard. So, the extra
checking will be equivalent to the case when we simply drop the place from the
list.

*Main> fastSolveMaze smallMaze (0,0) (3,2)
[E,N,E,S,E,N,N]
(0.00 secs, 122,584 bytes)
We notice that we use less memory than we did for the simple solveMaze
function. That might be counter-intuitive as we need an extra argument here,
which is visited, but actually now we have a smaller list argument as we
get rid of the cases when we go N,S,N,S,N,S... for example.

*Main> fastSolveMaze largeMaze (0,0) (22,21)
[N,N,N,N,N,N,N,N,N,E,E,E,N,W,W,W,N,E,E,E,N,W,W,W,N,E,E,E,E,E,N,N,N,W,S,S,W,W,W,W,N,N,N,E,S,S,E,E,N,W,N,N,W,W,N,E,E,E,E,E,E,N,W,W,W,W,W,W,N,E,E,E,E,E,E,E,S,S,S,S,E,E,N,N,N,N,E,E,E,E,S,W,W,W,S,S,S,E,N,N,E,E,E,S,W,W,S,S,W,W,W,W,W,S,E,E,E,S,W,W,W,S,S,S,E,S,S,S,E,N,N,N,E,S,S,S,S,W,W,W,S,E,E,E,S,W,W,W,S,E,E,E,E,S,S,E,E,E,E,E,E,E,S,E,E,E,N,W,W,N,N,N,E,S,S,E,E,N,W,N,E,N,N,W,S,W,W,W,W,S,W,N,N,N,W,W,W,N,N,N,E,S,S,E,N,N,N,W,W,N,N,N,N,N,E,S,S,S,S,E,E,E,E,E,E,E,S,W,W,W,W,W,S,E,E,E,E,E,E,N,N,N,W,W,W,W,N,E,E,N,W,W,N,E,E,N,W,W,W,N,N,N,E,S,S,E,N,N,E,E,E]
(0.09 secs, 12,190,208 bytes)

*Main> fastSolveMaze impossibleMaze (0,0) (2,2)
*** Exception: Impossible pathing
CallStack (from HasCallStack):
  error, called at Main.lhs:231:19 in main:Main

So, we solve the largeMaze problem in less than a second and we got
the error we wanted for the impossibleMaze case.

Now, let's check the function on empty mazes too:

*Main> fastSolveMaze (makeMaze (3,3) []) (0,0) (2,2)
[N,N,E,E]
(0.00 secs, 122,352 bytes)

*Main> fastSolveMaze (makeMaze (11,11) []) (0,0) (10,10)
[N,N,N,N,N,N,N,N,N,N,E,E,E,E,E,E,E,E,E,E]
(0.02 secs, 1,362,256 bytes)

Now, we get the idea that it definitely is more efficient (recall
that the initial solveMaze function couldn't do the task for a 6x6 empty
maze).

*Main> fastSolveMaze (makeMaze (51,51) []) (0,0) (50,50)
[N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E]
(0.57 secs, 223,898,264 bytes)

It also works for 51x51 empty mazes in under a second.

*Main> fastSolveMaze (makeMaze (101,101) []) (0,0) (100,100)
[N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E]
(6.04 secs, 3,124,711,800 bytes)

In this case, for a 101x101 empty maze, it needs 6 seconds to do the task.
We will compare these results with the next implementations to see if
we optimize the times or not.
======================================================================

Some test mazes.  In both cases, the task is to find a path from the bottom
left corner to the top right.

First a small one

> smallMaze :: Maze
> smallMaze =
>   let walls = [((0,0), N), ((2,2), E), ((2,1),E), ((1,0),E),
>                ((1,2), E), ((1,1), N)]
>   in makeMaze (4,3) walls

Now a large one.  Define a function to produce a run of walls:

> run (x,y) n E = [((x,y+i),E) | i <- [0..n-1]]
> run (x,y) n N = [((x+i,y),N) | i <- [0..n-1]]

And here is the maze.

> largeMaze :: Maze
> largeMaze =
>   let walls =
>         run (0,0) 3 E ++ run (1,1) 3 E ++ [((1,3),N)] ++ run (0,4) 5 E ++
>         run (2,0) 5 E ++ [((2,4),N)] ++ run (1,5) 3 E ++
>         run (1,8) 3 N ++ run (2,6) 3 E ++
>         run (3,1) 7 E ++ run (4,0) 4 N ++ run (4,1) 5 E ++ run (5,2) 3 N ++
>         run (4,6) 2 N ++ run (5,4) 3 E ++ run (6,3) 5 N ++ run (8,0) 4 E ++
>         run (6,1) 3 N ++ run (0,9) 3 N ++ run (1,10) 3 N ++ run (0,11) 3 N ++
>         run (1,12) 6 N ++ run (3,9) 4 E ++ run (4,11) 2 N ++
>         run (5,9) 3 E ++ run (4,8) 3 E ++ run (5,7) 5 N ++ run (6,4) 9 E ++
>         run (7,5) 3 N ++ run (8,4) 4 N ++ run (8,6) 3 N ++ run (10,5) 7 E ++
>         run (9,8) 3 E ++ run (8,9) 3 E ++ run (7,8) 3 E ++ run (8,11) 3 N ++
>         run (0,13) 5 N ++ run (4,14) 2 E ++ run (0,15) 2 E ++
>         run (1,14) 3 N ++ run (3,15) 2 E ++ run (0,17) 2 N ++
>         run (1,16) 2 E ++ run (2,15) 1 N ++ run (3,16) 3 N ++
>         run (2,17) 2 E ++ run (1,18) 6 N ++ run (4,17) 3 N ++
>         run (6,14) 7 E ++ run (5,13) 4 E ++ run (7,12) 2 E ++
>         run (8,13) 3 N ++ run (7,14) 3 N ++ run (10,14) 2 E ++
>         run (8,15) 5 N ++ run (7,16) 5 N ++ run (9,1) 2 E ++
>         run (10,0) 12 N ++ run (21,1) 1 E ++ run (10,2) 2 E ++
>         run (11,1) 7 N ++ run (17,1) 1 E ++ run (11,3) 3 E ++
>         run (12,2) 7 N ++ run (18,2) 2 E ++ run (19,1) 2 N ++
>         run (15,3) 3 N ++ run (14,4) 3 E ++ run (13,3) 3 E ++
>         run (12,4) 3 E ++ run (12,6) 3 N ++ run (11,7) 8 E ++
>         run (9,12) 3 N ++ run (12,14) 1 N ++ run (12,8) 10 E ++
>         run (0,19) 6 N ++ run (1,20) 6 N ++ run (7,18) 8 E ++
>         run (8,17) 1 N ++ run (8,18) 3 E ++ run (9,17) 4 E ++
>         run (10,18) 2 E ++ run (11,17) 2 E ++ run (10,20) 3 N ++
>         run (11,19) 3 N ++ run (12,18) 2 N ++ run (13,17) 2 N ++
>         run (13,13) 4 E ++ run (14,12) 7 N ++ run (13,11) 2 N ++
>         run (14,10) 2 E ++ run (13,9)2 E ++ run (14,8) 3 N ++
>         run (13,7) 3 N ++ run (15,5) 3 E ++ run (16,6) 3 E ++
>         run (18,5) 4 N ++ run (16,4) 2 N ++ run (13,20) 2 E ++
>         run (14,18) 4 E ++ run (20,2) 3 N ++ run (19,3) 2 E ++
>         run (18,4) 2 E ++ run (23,4) 1 E ++ run (22,4) 1 N ++
>         run (21,3) 1 N ++ run (20,4) 2 E ++ run (17,6) 4 N ++
>         run (20,7) 2 E ++ run (21,7) 2 N ++ run (21,6) 1 E ++
>         run (15,9) 1 E ++ run (17,8) 2 E ++ run (18,7) 2 E ++
>         run (19,8) 2 E ++ run (21,9) 1 E ++ run (16,9) 6 N ++
>         run (16,10) 7 N ++ run (15,11) 2 E ++ run (17,11) 5 N ++
>         run (14,14) 3 E ++ run (15,15) 6 E ++ run (17,14) 4 E ++
>         run (16,18) 4 E ++ run (15,17) 1 N ++ run (17,17) 3 N ++
>         run (15,13) 7 N ++ run (21,12) 2 E ++ run (16,16) 1 N ++
>         run (16,14) 1 N ++ run (17,15) 3 N ++ run (19,14) 4 N ++
>         run (20,15) 5 E ++ run (19,16) 2 N ++ run (21,16) 5 E ++
>         run (17,19) 2 E ++ run (18,20) 2 E ++ run (19,19) 2 E ++
>         run (18,18) 2 N ++ run (20,20) 3 N
>   in makeMaze (23,22) walls

And now an impossible maze

> impossibleMaze :: Maze
> impossibleMaze =
>   let walls = [((0,1), E), ((1,0),N), ((1,2), E), ((2,1), N)]
>   in makeMaze (3,3) walls

> smallEmptyMaze :: Maze
> smallEmptyMaze = makeMaze (21,21) []

> largeEmptyMaze :: Maze
> largeEmptyMaze = makeMaze (101,101) []

======================================================================

Question 5
----------

The new functions are created in the MyMaze.lhs file.
Here, we test the results to see if the time has improved or not.

*Main> fastSolveMaze smallMaze (0,0) (3,2)
[E,N,E,S,E,N,N]
(0.00 secs, 125,336 bytes)

*Main> fastSolveMaze largeMaze (0,0) (22,21)
[N,N,N,N,N,N,N,N,N,E,E,E,N,W,W,W,N,E,E,E,N,W,W,W,N,E,E,E,E,E,N,N,N,W,S,S,W,W,W,W,N,N,N,E,S,S,E,E,N,W,N,N,W,W,N,E,E,E,E,E,E,N,W,W,W,W,W,W,N,E,E,E,E,E,E,E,S,S,S,S,E,E,N,N,N,N,E,E,E,E,S,W,W,W,S,S,S,E,N,N,E,E,E,S,W,W,S,S,W,W,W,W,W,S,E,E,E,S,W,W,W,S,S,S,E,S,S,S,E,N,N,N,E,S,S,S,S,W,W,W,S,E,E,E,S,W,W,W,S,E,E,E,E,S,S,E,E,E,E,E,E,E,S,E,E,E,N,W,W,N,N,N,E,S,S,E,E,N,W,N,E,N,N,W,S,W,W,W,W,S,W,N,N,N,W,W,W,N,N,N,E,S,S,E,N,N,N,W,W,N,N,N,N,N,E,S,S,S,S,E,E,E,E,E,E,E,S,W,W,W,W,W,S,E,E,E,E,E,E,N,N,N,W,W,W,W,N,E,E,N,W,W,N,E,E,N,W,W,W,N,N,N,E,S,S,E,N,N,E,E,E]
(0.05 secs, 12,757,488 bytes)

Here, we can definitely see an improvement from the previous representation
of mazes. The time is almost halved.

*Main> fastSolveMaze impossibleMaze (0,0) (2,2)
*** Exception: Impossible pathing
CallStack (from HasCallStack):
  error, called at Main.lhs:231:19 in main:Main

*Main> fastSolveMaze (makeMaze (51,51) []) (0,0) (50,50)
[N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E]
(0.43 secs, 224,503,984 bytes)

*Main> fastSolveMaze (makeMaze (101,101) []) (0,0) (100,100)
[N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E]
(5.25 secs, 3,127,286,376 bytes)

 There is an improvement here too, but we can see that the memory usage
 is the same, as we basically used the same number of walls, but split into
 four lists.

======================================================================

Question 6
----------

> exampleMaze1 :: Maze
> exampleMaze1 =
>   let walls = [((i,j),N) | j <- [0,2..19], i <- [0..19]] -- even lines
>               ++ [((i,j),N) | j <- [1,3..19] , i <- [1..20]] -- odd lines
>   in makeMaze (21,21) walls

*Main> drawMaze exampleMaze1
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|                                                              |
+  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|                                                              |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+  +
|                                                              |
+  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|                                                              |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+  +
|                                                              |
+  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|                                                              |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+  +
|                                                              |
+  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|                                                              |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+  +
|                                                              |
+  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|                                                              |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+  +
|                                                              |
+  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|                                                              |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+  +
|                                                              |
+  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|                                                              |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+  +
|                                                              |
+  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|                                                              |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+  +
|                                                              |
+  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|                                                              |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+  +
|                                                              |
+  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|                                                              |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+  +
|                                                              |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

> exampleMaze2 :: Maze
> exampleMaze2 =
>   let walls =     filter p [((i,j),S) | i <- [0..20], j <- [0..20]]
>                ++ filter q [((i,j),W) | i <- [0..20], j <- [0..20]]
>                ++ filter s [((i,j),S) | i <- [0..20], j <- [0..20]]
>                ++ filter t [((i,j),W) | i <- [0..20], j <- [0..20]]
>   in makeMaze (21,21) walls
>      where p ((i,j),S) = (i>=j) && (i<=20-j) -- the walls from S
>            q ((i,j),W) = (j>=i) && (j<=20-i) -- the walls from W
>            s ((i,j),S) = (j>i) && (i>=21-j) -- the walls from N
>            t ((i,j),W) = (i>j) && (j>=21-i) -- the walls from E

*Main> drawMaze exampleMaze2
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|                                                              |
+  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+  +
|  |                                                        |  |
+  +  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+  +  +
|  |  |                                                  |  |  |
+  +  +  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+  +  +  +
|  |  |  |                                            |  |  |  |
+  +  +  +  +--+--+--+--+--+--+--+--+--+--+--+--+--+  +  +  +  +
|  |  |  |  |                                      |  |  |  |  |
+  +  +  +  +  +--+--+--+--+--+--+--+--+--+--+--+  +  +  +  +  +
|  |  |  |  |  |                                |  |  |  |  |  |
+  +  +  +  +  +  +--+--+--+--+--+--+--+--+--+  +  +  +  +  +  +
|  |  |  |  |  |  |                          |  |  |  |  |  |  |
+  +  +  +  +  +  +  +--+--+--+--+--+--+--+  +  +  +  +  +  +  +
|  |  |  |  |  |  |  |                    |  |  |  |  |  |  |  |
+  +  +  +  +  +  +  +  +--+--+--+--+--+  +  +  +  +  +  +  +  +
|  |  |  |  |  |  |  |  |              |  |  |  |  |  |  |  |  |
+  +  +  +  +  +  +  +  +  +--+--+--+  +  +  +  +  +  +  +  +  +
|  |  |  |  |  |  |  |  |  |        |  |  |  |  |  |  |  |  |  |
+  +  +  +  +  +  +  +  +  +  +--+  +  +  +  +  +  +  +  +  +  +
|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
+  +  +  +  +  +  +  +  +  +  +--+  +  +  +  +  +  +  +  +  +  +
|  |  |  |  |  |  |  |  |  |        |  |  |  |  |  |  |  |  |  |
+  +  +  +  +  +  +  +  +  +--+--+--+  +  +  +  +  +  +  +  +  +
|  |  |  |  |  |  |  |  |              |  |  |  |  |  |  |  |  |
+  +  +  +  +  +  +  +  +--+--+--+--+--+  +  +  +  +  +  +  +  +
|  |  |  |  |  |  |  |                    |  |  |  |  |  |  |  |
+  +  +  +  +  +  +  +--+--+--+--+--+--+--+  +  +  +  +  +  +  +
|  |  |  |  |  |  |                          |  |  |  |  |  |  |
+  +  +  +  +  +  +--+--+--+--+--+--+--+--+--+  +  +  +  +  +  +
|  |  |  |  |  |                                |  |  |  |  |  |
+  +  +  +  +  +--+--+--+--+--+--+--+--+--+--+--+  +  +  +  +  +
|  |  |  |  |                                      |  |  |  |  |
+  +  +  +  +--+--+--+--+--+--+--+--+--+--+--+--+--+  +  +  +  +
|  |  |  |                                            |  |  |  |
+  +  +  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+  +  +  +
|  |  |                                                  |  |  |
+  +  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+  +  +
|  |                                                        |  |
+  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+  +
|                                                              |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

> exampleMaze3 :: Maze
> exampleMaze3 =
>   let walls = [((j,i),E) | j <- [0,2..19], i <- [0..19]] -- even columns
>               ++ [((j,i),E) | j <- [1,3..19] , i <- [1..20]] -- odd columns
>   in makeMaze (21,21) walls

*Main> drawMaze exampleMaze3
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|     |     |     |     |     |     |     |     |     |     |  |
+  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +
|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
+  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +
|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
+  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +
|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
+  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +
|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
+  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +
|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
+  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +
|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
+  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +
|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
+  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +
|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
+  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +
|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
+  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +
|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
+  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +
|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
+  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +
|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
+  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +
|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
+  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +
|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
+  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +
|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
+  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +
|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
+  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +
|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
+  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +
|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
+  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +
|  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
+  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +  +
|  |     |     |     |     |     |     |     |     |     |     |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

======================================================================

Question 7
----------

As the work is done in BinMaze.lhs, we only check if the time to solve
our mazes has decreased or not.

*Main> fastSolveMaze largeMaze (0,0) (22,21)
[N,N,N,N,N,N,N,N,N,E,E,E,N,W,W,W,N,E,E,E,N,W,W,W,N,E,E,E,E,E,N,N,N,W,S,S,W,W,W,W,N,N,N,E,S,S,E,E,N,W,N,N,W,W,N,E,E,E,E,E,E,N,W,W,W,W,W,W,N,E,E,E,E,E,E,E,S,S,S,S,E,E,N,N,N,N,E,E,E,E,S,W,W,W,S,S,S,E,N,N,E,E,E,S,W,W,S,S,W,W,W,W,W,S,E,E,E,S,W,W,W,S,S,S,E,S,S,S,E,N,N,N,E,S,S,S,S,W,W,W,S,E,E,E,S,W,W,W,S,E,E,E,E,S,S,E,E,E,E,E,E,E,S,E,E,E,N,W,W,N,N,N,E,S,S,E,E,N,W,N,E,N,N,W,S,W,W,W,W,S,W,N,N,N,W,W,W,N,N,N,E,S,S,E,N,N,N,W,W,N,N,N,N,N,E,S,S,S,S,E,E,E,E,E,E,E,S,W,W,W,W,W,S,E,E,E,E,E,E,N,N,N,W,W,W,W,N,E,E,N,W,W,N,E,E,N,W,W,W,N,N,N,E,S,S,E,N,N,E,E,E]
(0.05 secs, 15,026,368 bytes)

*Main> fastSolveMaze (makeMaze (51,51) []) (0,0) (50,50)
[N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E]
(0.39 secs, 238,293,552 bytes)

*Main> fastSolveMaze (makeMaze (101,101) []) (0,0) (100,100)
[N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,N,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E,E]
(4.95 secs, 3,191,835,632 bytes)

We observe that we have faster solutions than before.
