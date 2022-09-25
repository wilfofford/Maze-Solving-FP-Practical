import Geography
import Maze
import Data.List
import GHC.IO.Encoding
import System.Win32.Console
import System.Random
import Control.Monad.State
import System.Environment

randomNum :: (RandomGen g) => State g Double
randomNum = state random

main :: IO()
main = do 
    [x, y, prob] <- getArgs
    setLocaleEncoding utf8
    setConsoleOutputCP 65001
    putStrLn "Solving the large maze:"
    drawMazeSolved largeMaze
    putStrLn "Solving a random maze"
    runRandom $ drawRandomSolved (read x, read y) (read prob)

type Path = [Direction]
type Dir = Direction 

turner :: (Dir,Dir) -> String
turner (N,N) = "\ESC[92m┃ \ESC[0m"
turner (S,S) = "\ESC[92m┃ \ESC[0m"
turner (W,W) = "\ESC[92m━━\ESC[0m"
turner (E,E) = "\ESC[92m━━\ESC[0m"
turner (N,W) = "\ESC[92m┓ \ESC[0m" 
turner (N,E) = "\ESC[92m┏━\ESC[0m"
turner (S,W) = "\ESC[92m┛ \ESC[0m" 
turner (S,E) = "\ESC[92m┗━\ESC[0m" 
turner (E,N) = "\ESC[92m┛ \ESC[0m" 
turner (E,S) = "\ESC[92m┓ \ESC[0m" 
turner (W,N) = "\ESC[92m┗━\ESC[0m" 
turner (W,S) = "\ESC[92m┏━\ESC[0m"

startTurn :: Dir -> String
startTurn N = "\ESC[92m┃ \ESC[0m"
startTurn S = "\ESC[92m┃ \ESC[0m"
startTurn E = "\ESC[92m━━\ESC[0m"
startTurn W = "\ESC[92m━━\ESC[0m"

run :: Place -> Int -> Direction -> [Wall]
run (x,y) n E = [((x,y+i),E) | i <- [0..n-1]]
run (x,y) n N = [((x+i,y),N) | i <- [0..n-1]]

drawMaze :: Maze -> IO()
drawMaze = putStrLn.mazeString


horline :: Maze -> Int -> String
horline maze row = '+':([0..x-1] >>= \i -> (if hasWall maze (i,row) N then "--" else "  ")++"+")
    where (x,y) = sizeOf maze

verline :: Maze -> Int -> String
verline maze row = [0..x] >>= \i -> ((if hasWall maze (i,row) W then "|" else " ")++"  ")
    where (x,y) = sizeOf maze

mazeString :: Maze -> String
mazeString maze = ([1..y] >>= \row -> ((horline maze (y-row))++"\n"++(verline maze (y-row))++"\n"))++horline maze (-1)
    where (x,y) = sizeOf maze

formNewPath :: Maze -> (Place,Path) -> [(Place, Path)]
formNewPath maze ((x,y),path) = filter (\p -> not $ hasWall maze (x,y) (head $ snd p)) [(move dir (x,y), dir:path) | dir <-[N,S,E,W]]

expandSearch :: Maze -> [(Place,Path)] -> [(Place,Path)]
expandSearch maze xs = xs >>= (formNewPath maze)

reached :: Place -> [(Place, Path)] -> [Path]
reached target xs = map snd (filter (\x -> fst x == target) xs)

solveMazeIter :: Maze -> Place -> [(Place,Path)] -> Path
solveMazeIter maze target xs = case reached target xs of
    y:ys -> y
    [] -> solveMazeIter maze target $ expandSearch maze xs

solveMazeTarget :: Maze -> Place -> Path
solveMazeTarget maze target = reverse $ solveMazeIter maze target [((0,0),[])]

solveMaze :: Maze -> Path
solveMaze maze = solveMazeTarget maze (x-1,y-1)
    where (x,y) = sizeOf maze

fastExpandSearch :: Maze -> ([(Place,Path)],[Place]) -> ([(Place,Path)],[Place])
fastExpandSearch maze (partial, visited) = (newPartial, newPlaces ++ visited)
    where newPartial = filter (\x -> not $ fst x `elem` visited) $ expandSearch maze partial
          newPlaces = map fst newPartial

fastSolveMazeIter :: Maze -> Place -> [(Place,Path)] -> [Place] -> Maybe Path
fastSolveMazeIter _ _ [] _ = Nothing
fastSolveMazeIter maze target partial visited = case reached target partial of
    y:ys -> Just y
    [] -> fastSolveMazeIter maze target newPartial newVisited
    where (newPartial, newVisited) = fastExpandSearch maze (partial,visited)

fastSolveMazeTarget :: Maze -> Place -> Maybe Path
fastSolveMazeTarget maze target = fmap reverse $ fastSolveMazeIter maze target [((0,0),[])] [(0,0)]

fastSolveMaze :: Maze -> Maybe Path
fastSolveMaze maze = fastSolveMazeTarget maze (x-1,y-1)
    where (x,y) = sizeOf maze


pathToPlaces :: Path -> [Place]
pathToPlaces path = foldl (\acc dir -> (move dir $ head acc):acc) [(0,0)] path

listPairs :: [a] -> [(a,a)]
listPairs (x:y:xs) = (x,y):(listPairs (y:xs))
listPairs xs = []

pathToPipes :: Path -> [(Place,String)]
pathToPipes path = zip (reverse $ pathToPlaces path) pipe
    where pipe = (startTurn $ head path):(map turner $ listPairs path) ++ [startTurn $ last path]

-- verlineSolved :: Maze -> Int -> String
-- verlineSolved maze row = concat [(if hasWall maze (i,row) W then "|" else " ") ++ if (i,row) `elem` solution then "\ESC[92m██\ESC[0m" else "  " | i<-[0..x]]
--     where (x,y) = sizeOf maze
--           Just solution = fmap pathToPlaces $ fastSolveMaze maze

verlineSolved :: Maze -> Int -> String
verlineSolved maze row = [0..x] >>= \i -> ((if hasWall maze (i,row) W then "|" else " ") ++ if (i,row) `elem` (map fst pipes) then turnChar (i,row) else "  ")
    where (x,y) = sizeOf maze
          Just pipes = fmap pathToPipes $ fastSolveMaze maze
          turnChar x = let Just answer = lookup x pipes in answer


mazeStringSolved :: Maze -> Maybe String
mazeStringSolved maze = if fastSolveMaze maze == Nothing then Nothing else
    Just $ ([1..y] >>= \row -> ((horline maze (y-row))++"\n"++(verlineSolved maze (y-row))++"\n"))++horline maze (-1)
    where (x,y) = sizeOf maze

drawMazeSolved :: Maze -> IO ()
drawMazeSolved maze = if mazeStringSolved maze == Nothing then putStrLn "No Solution" else
    putStrLn solvedstring
    where Just solvedstring = mazeStringSolved maze

randomMaze :: (RandomGen g) => Size -> Double -> State g Maze
randomMaze (x,y) prob = fmap (makeMaze (x,y)) randomWalls
    where isSmall = (<prob) 
          randomWalls :: (RandomGen g) => State g [Wall]
          randomWalls = filterM (\t -> fmap isSmall randomNum) [((i,j),dir) | i <- [0..x-2], j <- [0..y-1], dir <- [N,E]]

drawRandomMaze :: (RandomGen g) => Size -> Double -> State g (IO ())
drawRandomMaze size prob = fmap drawMaze $ randomMaze size prob 


randomSolvableMaze :: (RandomGen g) => Size -> Double -> State g Maze
randomSolvableMaze size prob = do
    maze <- randomMaze size prob
    if not (fastSolveMaze maze == Nothing ) then return maze else randomSolvableMaze size prob


drawRandomSolvableMaze :: (RandomGen g) => Size -> Double -> State g (IO ())
drawRandomSolvableMaze size prob = fmap drawMaze $ randomSolvableMaze size prob 

drawRandomSolved :: (RandomGen g) => Size -> Double -> State g (IO ())
drawRandomSolved size prob = fmap drawMazeSolved $ randomSolvableMaze size prob

runRandom :: State StdGen (IO a) -> IO a
runRandom random = join nested where
    nested = do
    gen <- getStdGen
    return (fst $ runState random gen)

largeMaze :: Maze 
largeMaze =
   let walls = 
         run (0,0) 3 E ++ run (1,1) 3 E ++ [((1,3),N)] ++ run (0,4) 5 E ++
         run (2,0) 5 E ++ [((2,4),N)] ++ run (1,5) 3 E ++
         run (1,8) 3 N ++ run (2,6) 3 E ++
         run (3,1) 7 E ++ run (4,0) 4 N ++ run (4,1) 5 E ++ run (5,2) 3 N ++
         run (4,6) 2 N ++ run (5,4) 3 E ++ run (6,3) 5 N ++ run (8,0) 4 E ++
         run (6,1) 3 N ++ run (0,9) 3 N ++ run (1,10) 3 N ++ run (0,11) 3 N ++
         run (1,12) 6 N ++ run (3,9) 4 E ++ run (4,11) 2 N ++
         run (5,9) 3 E ++ run (4,8) 3 E ++ run (5,7) 5 N ++ run (6,4) 9 E ++
         run (7,5) 3 N ++ run (8,4) 4 N ++ run (8,6) 3 N ++ run (10,5) 7 E ++
         run (9,8) 3 E ++ run (8,9) 3 E ++ run (7,8) 3 E ++ run (8,11) 3 N ++
         run (0,13) 5 N ++ run (4,14) 2 E ++ run (0,15) 2 E ++ 
         run (1,14) 3 N ++ run (3,15) 2 E ++ run (0,17) 2 N ++ 
         run (1,16) 2 E ++ run (2,15) 1 N ++ run (3,16) 3 N ++
         run (2,17) 2 E ++ run (1,18) 6 N ++ run (4,17) 3 N ++ 
         run (6,14) 7 E ++ run (5,13) 4 E ++ run (7,12) 2 E ++
         run (8,13) 3 N ++ run (7,14) 3 N ++ run (10,14) 2 E ++
         run (8,15) 5 N ++ run (7,16) 5 N ++ run (9,1) 2 E ++
         run (10,0) 12 N ++ run (21,1) 1 E ++ run (10,2) 2 E ++
         run (11,1) 7 N ++ run (17,1) 1 E ++ run (11,3) 3 E ++
         run (12,2) 7 N ++ run (18,2) 2 E ++ run (19,1) 2 N ++
         run (15,3) 3 N ++ run (14,4) 3 E ++ run (13,3) 3 E ++
         run (12,4) 3 E ++ run (12,6) 3 N ++ run (11,7) 8 E ++ 
         run (9,12) 3 N ++ run (12,14) 1 N ++ run (12,8) 10 E ++
         run (0,19) 6 N ++ run (1,20) 6 N ++ run (7,18) 8 E ++
         run (8,17) 1 N ++ run (8,18) 3 E ++ run (9,17) 4 E ++ 
         run (10,18) 2 E ++ run (11,17) 2 E ++ run (10,20) 3 N ++
         run (11,19) 3 N ++ run (12,18) 2 N ++ run (13,17) 2 N ++
         run (13,13) 4 E ++ run (14,12) 7 N ++ run (13,11) 2 N ++
         run (14,10) 2 E ++ run (13,9)2 E ++ run (14,8) 3 N ++ 
         run (13,7) 3 N ++ run (15,5) 3 E ++ run (16,6) 3 E ++
         run (18,5) 4 N ++ run (16,4) 2 N ++ run (13,20) 2 E ++
         run (14,18) 4 E ++ run (20,2) 3 N ++ run (19,3) 2 E ++
         run (18,4) 2 E ++ run (23,4) 1 E ++ run (22,4) 1 N ++
         run (21,3) 1 N ++ run (20,4) 2 E ++ run (17,6) 4 N ++ 
         run (20,7) 2 E ++ run (21,7) 2 N ++ run (21,6) 1 E ++ 
         run (15,9) 1 E ++ run (17,8) 2 E ++ run (18,7) 2 E ++ 
         run (19,8) 2 E ++ run (21,9) 1 E ++ run (16,9) 6 N ++
         run (16,10) 7 N ++ run (15,11) 2 E ++ run (17,11) 5 N ++ 
         run (14,14) 3 E ++ run (15,15) 6 E ++ run (17,14) 4 E ++
         run (16,18) 4 E ++ run (15,17) 1 N ++ run (17,17) 3 N ++
         run (15,13) 7 N ++ run (21,12) 2 E ++ run (16,16) 1 N ++
         run (16,14) 1 N ++ run (17,15) 3 N ++ run (19,14) 4 N ++
         run (20,15) 5 E ++ run (19,16) 2 N ++ run (21,16) 5 E ++
         run (17,19) 2 E ++ run (18,20) 2 E ++ run (19,19) 2 E ++
         run (18,18) 2 N ++ run (20,20) 3 N
   in makeMaze (23,22) walls

impossibleMaze :: Maze
impossibleMaze =
      let walls = [((0,1), E), ((1,0),N), ((1,2), E), ((2,1), N)]
  in makeMaze (3,3) walls
  
smallMaze :: Maze
smallMaze = 
      let walls = [((0,0), N), ((2,2), E), ((2,1),E), ((1,0),E), 
               ((1,2), E), ((1,1), N)]
  in makeMaze (4,3) walls
