{-# LANGUAGE OverloadedStrings #-}


import Data.Text as Text
import System.IO

main :: IO ()


-- Tiles used in the game
wall, ground, storage, box, err :: Picture
wall _ 0 0 = "\ESC[30m\ESC[103m#\ESC[0m" 
wall d x y = d x y
ground _ 0 0 = "\ESC[107m \ESC[0m" 
ground d x y = d x y
storage _ 0 0 = "\ESC[42m.\ESC[0m"
storage d x y = d x y
box _ 0 0 = "\ESC[44m$\ESC[0m"
box d x y = d x y
err _ 0 0 = "\ESC[30m \ESC[0m"
err d x y = d x y


-- Picture of player
player :: Picture
player _ 0 0 = "\ESC[41m@\ESC[0m" 
player d x y = d x y

welcomeScreen :: Screen
welcomeScreen = "Sokoban in the Terminal.\n\n" ++
  "Controls:\n" ++
  "Use \ESC[31mWSAD\ESC[0m to move.\n" ++
  "Press \ESC[32mU\ESC[0m to undo.\n" ++
  "Press \ESC[33mR\ESC[m to reset\n\n" ++
  "Press \ESC[31mSpace\ESC[0m to Start!\n\n" ++
  "Good luck!\n"


screenCoords :: [Coord]
screenCoords = [(C x y) | x <- [-8..8], y <- [-28..28]]

hideCursor :: Screen
hideCursor = "\ESC[?25l"

showCursor :: Screen
showCursor = "\ESC[?25h"

----------
-- Data --
----------

-- Tile
data Tile = Wall | Ground | Storage | Box | Err deriving Eq
-- Direction
data Direction = R | U | L | D | NaN deriving (Eq, Enum)
-- Coordinations
data Coord = C Integer Integer deriving Eq
-- Move
data Move = M Coord Direction

data State = CC [Maze] Coord Direction [Coord] Integer

data SSState world = StartScreen | Running world

type MazeBoard = Coord -> Tile

type Player = Picture

data Maze = Maze Coord MazeBoard

data Interaction world = Interaction
  world
  (Event -> world -> world)
  (world -> Screen)

data Event = KeyPress String
type Screen = String

type DrawFun = Integer -> Integer -> String
type Picture = DrawFun -> DrawFun

blank :: Picture
blank = id

(&) :: Picture -> Picture -> Picture
(&) = (.)

instance Eq State where 
  (CC m c d b s) == (CC m2 c2 d2 b2 s2) =
    listLength m == listLength m2 &&
    c == c2 &&
    d == d2 &&
    b == b2 &&
    s == s2


--------------
-- Movement --
--------------

-- Function checking if we can move on the tile
correctTile :: Tile -> Bool
correctTile Ground = True
correctTile Storage = True
correctTile _ = False

drawTile :: Tile -> Picture
drawTile Wall = wall
drawTile Ground = ground
drawTile Storage = storage
drawTile Box = box
drawTile Err = err


trans :: Integer -> Integer -> Picture -> Picture
trans dx dy p f x y = p moved (x - dx) (y - dy) where
  moved :: DrawFun
  moved x y = f (x + dx) (y + dy)


-- Calculate new coordinates after direction
adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = (C (x+1) y)
adjacentCoord U (C x y) = (C x (y+1))
adjacentCoord L (C x y) = (C (x-1) y)
adjacentCoord D (C x y) = (C x (y-1))
adjacentCoord NaN coord = coord

-- Move picture by x and y
atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = trans x y pic


calcCoord :: Coord -> Direction -> Coord
calcCoord coord@(C x y) d
  | d == U = (C x (y+1))
  | d == D = (C x (y-1))
  | d == L = (C (x-1) y)
  | d == R = (C (x+1) y)
  | otherwise = coord


textToDir :: [Char] -> Direction
textToDir key
  | key == "d" = R
  | key == "w" = U
  | key == "a" = L
  | key == "s" = D
  | otherwise = NaN



-----------
-- Mazes --
-----------

maze1 :: Maze
maze1 = Maze initialCoord maze where
  initialCoord = C 0 1
  maze (C x y)
    | abs x > 4  || abs y > 4  = Err
    | abs x == 4 || abs y == 4 = Wall
    | x ==  2 && y <= 0        = Wall
    | x ==  3 && y <= 0        = Storage
    | x >= -2 && y == 0        = Box
    | otherwise                = Ground
    
    
maze2 :: Maze
maze2 = Maze initialCoord maze where
  initialCoord = C 0 1
  maze (C x y)
    | abs x > 4  || abs y > 4  = Err
    | abs x == 4 || abs y == 4 = Wall
    | x ==  2 && abs y <= 2    = Storage
    | x == -2 && abs y <= 2    = Box
    | otherwise                = Ground
  
  
maze3 :: Maze
maze3 = Maze initialCoord maze where
  initialCoord = C 0 1
  maze (C x y)
    | abs x > 4  || abs y > 4  = Err
    | abs x == 4 || abs y == 4 = Wall
    | x ==  2 && y <= 0        = Wall
    | x ==  3 && y <= 0        = Storage
    | x >= -2 && y == 0        = Box
    | x == 3 && y == 1         = Wall
    | otherwise                = Ground
    
    
maze4 :: Maze
maze4 = Maze initialCoord maze where
  initialCoord = C 0 1
  maze (C x y)
    | abs x > 4  || abs y > 4  = Err
    | abs x == 4 || abs y == 4 = Wall
    | x ==  2 && abs y <= 2    = Wall
    | x == -2 && abs y <= 2    = Wall
    | abs x <= 1 && y == 0     = Box
    | otherwise                = Ground
    

mazes, badMazes :: [Maze]
mazes = [maze1, maze2]
badMazes = [maze3, maze4]

--------------------
-- List Functions --
--------------------

elemList :: Eq a => a -> [a] -> Bool
elemList x y = foldList (\a b -> a == x || b) False y

appendList :: [a] -> [a] -> [a]
appendList x y = foldList (\y x -> y:x) x $ Prelude.reverse y

listLength :: [a] -> Integer
listLength x = foldList (\x sum -> sum+1) 0 x

filterList :: (a -> Bool) -> [a] -> [a]
filterList f x = foldList (\x b -> if f x then x:b else b) [] x

safe_nth :: [a] -> Int -> Maybe a -- b ~ (Int -> MAybe a)
safe_nth = foldList f z where
  z :: Int -> Maybe a
  z _ = Nothing

  f :: a -> (Int -> Maybe a) -> (Int -> Maybe a)
  f a b 0 = Just a
  f a b n = b (n-1)

mapList :: (a -> b) -> [a] -> [b]
mapList f x = foldList (\x b -> (f x):b) [] x

andList :: [Bool] -> Bool
andList x = foldList (&&) True x

allList :: (a-> Bool) -> [a] -> Bool
allList f x = foldList (\x b -> f x && b) True x

foldList :: (a -> b -> b) -> b -> [a] -> b
foldList _ b [] = b
foldList f b (x:xs) = foldList f (f x b) xs

foldPictures :: [Picture] -> Picture
foldPictures p = foldList (&) blank p

------------
-- Graphs --
------------

dfs :: Eq a => a -> [a] -> (a -> [a]) -> [a]
dfs v visited neighbours = foldList (\vertex list -> if elemList vertex list then list else dfs vertex list neighbours) (v:visited) $ neighbours v

isGraphClosed :: Eq a => a -> (a -> [a]) -> (a -> Bool) -> Bool
isGraphClosed initial neighbours isOk = allList isOk (dfs initial [] neighbours)

reachable :: Eq a => a -> a -> (a -> [a]) -> Bool
reachable v initial neighbours = elemList v (dfs initial [] neighbours)

allReachable :: Eq a => [a] -> a -> (a -> [a]) -> Bool
allReachable vs initial neighbours = allList (\v -> reachable v initial neighbours) vs 

isClosed :: Maze -> Bool
isClosed (Maze initialCoord maze) = correctInitialCoord && noBlanks where
  correctInitialCoord = correctTile (maze initialCoord)
  noBlanks = isGraphClosed initialCoord neighbours (\c -> maze c /= Err)
  neighbours c = if elemList (maze c) [Ground, Storage] then (mapList(\d -> adjacentCoord d c) [R ..])  else []

isSane :: Maze -> Bool
isSane (Maze initialCoord maze) = storages >= boxes where
  reached = dfs initialCoord [] neighbours
  storages = listLength (filterList (\a -> maze a == Storage) reached)
  boxes = listLength (filterList (\a -> maze a == Box) reached) 
  neighbours c = if elemList (maze c) [Ground, Storage] then (mapList(\d -> adjacentCoord d c) [R ..])  else []


initialState :: State
initialState = CC mazes (C 0 1) U (initialBoxes (Prelude.head mazes)) 0


initialBoxes :: Maze -> [Coord]
initialBoxes m = getBoxesFromMaze m


getBoxesFromMaze :: Maze -> [Coord]
getBoxesFromMaze (Maze initialCoord maze) = filterList(\x -> maze x == Box) reached where
  reached = dfs initialCoord [] neighbours
  neighbours c = if elemList (maze c) [Ground, Storage] then (mapList(\d -> adjacentCoord d c) [R ..])  else []


removeBoxes :: MazeBoard -> MazeBoard
removeBoxes maze =
  let
    removeBox Box = Ground
    removeBox tile = tile
  in
    removeBox . maze

addBoxes :: [Coord] -> MazeBoard -> MazeBoard
addBoxes [] maze coord = maze coord
addBoxes (c:rest) maze coord@(C x2 y2)
  | c == coord = Box
  | otherwise = addBoxes rest maze coord


adjacentCoordIfAccesible :: Direction -> Coord -> MazeBoard -> Coord
adjacentCoordIfAccesible d coord@(C x y) m
  | upcomingTile == Ground || upcomingTile == Storage = newCoord
  | upcomingTile == Box && (furtherTile == Ground || furtherTile == Storage) = newCoord
  | otherwise = coord
  where 
    newCoord = calcCoord coord d
    upcomingTile = m newCoord
    furtherCoord = calcCoord newCoord d
    furtherTile = m furtherCoord


adjacentBoxes :: [Coord] -> Coord -> Direction -> [Coord]
adjacentBoxes boxes crd@(C x y) dir = 
  Prelude.map f boxes
  where 
    f coord@(C x1 y1) = 
      if coord == crd
        then calcCoord coord dir
        else coord


isWinningState :: State -> Bool
isWinningState (CC ((Maze initialCoord maze):_) coord dir boxes steps) = finished where
  finished = allList (\c -> maze c == Storage) boxes


draw :: State -> Screen
draw (CC [] _ _ _ _) = "Wygrałeś!!!\n\n"
  ++ "Aby rozpocząć grę od nowa naciśnij \ESC[32mU\ESC[0m a następnie \ESC[32mR\ESC[0m."
draw state@(CC currMazes@((Maze _ mazeBoard):rest) (C x1 y1) dir boxes steps) =
  if isWinningState state then
    "Poziom Ukończony, liczba ruchów: " 
    ++ (show steps) 
    ++ "\n\n" 
    ++ "Nacisnij \ESC[31mdowolny\ESC[0m przycisk aby przejsc dalej."
  else
    drawToTerminal $ ((movedPlayer & mazePicture) empty) where
      movedPlayer = trans x1 y1 (player)
      curMaze = addBoxes boxes $ removeBoxes mazeBoard
      mazePicture = pictureMaze curMaze 
      empty :: DrawFun 
      empty _ _ = " "
    

pictureMaze :: MazeBoard -> Picture
pictureMaze m = foldPictures [trans x y (drawTile (m c)) | c@(C x y) <- screenCoords] 


drawToTerminal :: DrawFun -> Screen
drawToTerminal f = Prelude.concat [drawWithEnter x y | y <- (Prelude.reverse [yMin..yMax]), x <- [xMin..(xMax+1)]] where
    yMin = -8
    yMax = 8
    xMin = -28
    xMax = 28
    drawWithEnter x y 
      | x == (xMax + 1) = "\n"
      | otherwise = f x y
 

handleEvent :: Event -> State -> State
handleEvent (KeyPress e)  state@(CC mazes@((Maze initialCoord mazeBoard):rest) coord direction boxes steps) = 
  if e == "r" then initialState
  else if isWinningState state then 
    if listLength rest == 0 then (CC rest (C 0 1) U [] 0)
    else (CC rest (C 0 1) U (initialBoxes $ Prelude.head rest) 0)
  else
    (CC mazes newCord dir (adjacentBoxes boxes newCord dir) (steps+1))
    where
     newCord = adjacentCoordIfAccesible dir coord curMaze
     dir     = textToDir e
     curMaze = addBoxes boxes (removeBoxes mazeBoard)    
handleEvent _ s = s


startScreenInteractionOf :: Interaction i -> Interaction (SSState i)
startScreenInteractionOf (Interaction state handle draw) = (Interaction state' handle' draw')
  where
    state' = StartScreen 
    handle' (KeyPress key) StartScreen
      | key == " " = Running state
    handle' _ StartScreen = StartScreen
    handle' e (Running s) = Running (handle e s)
    draw' StartScreen = welcomeScreen
    draw' (Running s) = draw s


data WithUndo a = WithUndo a [a]
withUndo :: Eq a => Interaction a -> Interaction (WithUndo a)
withUndo (Interaction state0 handle draw) = Interaction state0' handle' draw' where
    state0' = WithUndo state0 []
    handle' (KeyPress key) (WithUndo s stack) | key == "u"
      = case stack of s':stack' -> WithUndo s' stack'
                      []        -> WithUndo s []
    handle' e (WithUndo s stack)
       | s' == s = WithUndo s stack
       | otherwise = WithUndo (handle e s) (s:stack)
      where s' = handle e s
    draw' (WithUndo s _) = draw s


runInteraction :: Interaction i -> IO ()
runInteraction (Interaction s handle draw) = do 
  input <- getChar
  let newState = handle (KeyPress [input]) s
  putStr "\ESCc"
  putStr hideCursor
  putStr $ draw newState
  runInteraction (Interaction newState handle draw)

--- MAIN --
main = do
  hSetBuffering stdin NoBuffering
  putStr "\ESCc"
  putStr hideCursor
  putStr welcomeScreen
  runInteraction (startScreenInteractionOf (withUndo (Interaction initialState handleEvent draw)))
  putStr showCursor
