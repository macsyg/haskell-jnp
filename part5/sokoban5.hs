{-# LANGUAGE OverloadedStrings #-}
import System.IO
import Data.List
main :: Program
type Program = IO ()

main = runActivity (resettable (withUndo (withStartScreen (Activity initialState handleEvent draw))))

pictureOfBool :: Bool -> String
pictureOfBool x
  | x = "\ESC[30;42m "
  | otherwise = "\ESC[30;41m "

levels :: [String]
levels = mapList pictureOfBool (mapList isBoth (appendList mazes badMazes))

-- testy programu
--test = print (isGraphClosed (startCoord firstMaze) reachableNeighbours (okVertex firstMaze))
--test = print (allReachableVertices (reachableNeighbours (coordToTile fourthMaze)) (startCoord fourthMaze) [])
--test = print (isSane fourthMaze)
-- testy programu END

-- bloki i gracz

-- kolorowa wersja
-- ground, wall, storage, box, player, blank :: String
-- ground = "\ESC[92;40mo"
-- wall = "\ESC[30;47m#"
-- storage = "\ESC[93;40m."
-- box = "\ESC[33;40mo"
-- player = "\ESC[31;40mo"
-- blank = "\ESC[0m "
-- boxOnStorage = "\ESC[93;40mo"
-- lastBlank = ""
-- kolorowa wersja END

-- niekolorowa wersja
ground, wall, storage, box, player, blank :: String
ground = " "
wall = "#"
storage = "."
box = "$"
player = "@"
blank = " "
boxOnStorage = "*"
lastBlank = ""
-- niekolorowa wersja END

-- bloki i gracz END

-- stan gry
initialState :: State
initialState = S (startCoord (nth mazes 1)) initialDirection (getBoxCoords (nth mazes 1)) 1 0
-- stan gry END

-- poziomy 
firstMaze = Maze (C 1 (-1)) maze
    where maze c
            | abs (coordX c) > 4  || abs (coordY c) > 4  = Blank
            | abs (coordX c) == 4 || abs (coordY c) == 4 = Wall
            | coordX c ==  2 && coordY c <= 0        = Wall
            | coordX c ==  3 && coordY c <= 0        = Storage
            | coordX c >= -2 && coordY c == 0        = Box
            | otherwise                = Ground
  
secondMaze = Maze (C 1 (-1)) maze2
   where maze2 c
            | abs (coordX c) > 4  || abs (coordY c) > 4  = Blank
            | abs (coordX c) == 4 || abs (coordY c) == 4 = Wall
            | coordX c ==  2 && coordY c <= 0        = Wall
            | coordX c ==  3 && coordY c <= 0        = Storage
            | coordX c >= -2 && coordX c <= 1 && coordY c == 1  = Box
            | otherwise                = Ground
  
thirdMaze = Maze (C (-2) (-2)) maze3
    where maze3 (C x y)
              | abs x > 3 || abs y > 3 = Blank
              | abs x == 3 || abs y == 3 = Wall
              | y == 0 && x < 0 = Wall
              | abs y == 1 && x == -2 = Storage
              | y == 2 && x == 1 = Storage
              | y == -1 && x <= 1 && x >= 0 = Box
              | y == 1 && x == 1 = Box
              | otherwise = Ground
  
fourthMaze = Maze (C 1 (-1)) maze4
    where maze4 (C x y)
              | abs x > 3 || abs y > 3 = Blank
              | abs x == 3 || abs y == 3 = Wall
              | x == -2 || y == -2 = Wall
              | x == 2 && y == 2 = Wall
              | x == -1 && y <= 1 = Storage
              | x == 0 && (y == 0 || y == 1) = Box
              | x == 1 && y == 0 = Box
              | otherwise = Ground
  
firstBadMaze = Maze (C 1 (-1)) badMaze
  where badMaze (C x y)
              | abs x > 4  || abs y > 4  = Blank
              | x == -4 && y == 2 = Blank
              | abs x == 4 || abs y == 4 = Wall
              | x ==  2 && y <= 0        = Wall
              | x ==  3 && y <= 0        = Storage
              | x >= -2 && y == -3        = Box
              | otherwise                = Ground
  
secondBadMaze = Maze (C 1 (-1)) badMaze2
  where badMaze2 (C x y)
              | abs x > 4  || abs y > 4  = Blank
              | abs x == 4 || abs y == 4 = Wall
              | x ==  2 && y <= 0        = Wall
              | x ==  3 && y <= 1        = Storage
              | x >= -2 && y == -3        = Box
              | otherwise                = Ground
  
thirdBadMaze = Maze (C (-2) (-2)) badMaze3
  where badMaze3 (C x y)
                | abs x > 3 || abs y > 3 = Blank
                | y == -3 && (x == -1 || x == -2) = Ground
                | abs x == 3 || abs y == 3 = Wall
                | y == 0 && x < 0 = Wall
                | y == -1 && x == -1 = Wall
                | abs y == 1 && x == -2 = Storage
                | y == 2 && x == 1 = Storage
                | y == -1 && x <= 1 = Box
                | y == 1 && x == 1 = Box
                | otherwise = Ground
        
fourthBadMaze = Maze (C 3 (-2)) badMaze4
    where badMaze4 (C x y)
                  | abs x > 4 || y > 2 || y < -3              = Blank
                  | abs x == 4 || y == 2 || y == -3 || x == 0 = Wall
                  | x == -3 && y == 1                         = Storage
                  | x == 1 && y == 0                          = Box
                  | otherwise                                 = Ground
  
mazes = [firstMaze, secondMaze, thirdMaze, fourthMaze]
badMazes = [firstBadMaze, secondBadMaze, thirdBadMaze, fourthBadMaze]
numberOfMazes = 4
-- poziomy END

-- wprowadzone typy
data Tile = Wall | Ground | Storage | Box | Blank deriving (Eq, Show)
data Direction = R | U | L | D deriving Eq

dirList :: [Direction]
dirList = [U, R, D, L]

tilesToMove :: [Tile]
tilesToMove = [Ground, Storage, Box]

data Coord = C {
  coordX :: Integer,
  coordY :: Integer
} deriving (Eq, Show)

data State = S {
  playerCoord :: Coord,
  playerDir :: Direction,
  boxCoords :: [Coord],
  levelNumber :: Integer,
  numOfMoves :: Integer
} deriving Eq

data Activity world = Activity {
  actState  :: world,
  actHandle :: (Event -> world -> world),
  actDraw   :: (world -> Screen)
} 

data Level world = StartScreen | Running world deriving Eq

data Maze = Maze {
  startCoord :: Coord,
  coordToTile :: (Coord -> Tile)
}

data WithUndo a = WithUndo a [a]

data Event = KeyPress String

type DrawFun = Coord -> String
type Picture = DrawFun -> DrawFun
type Screen = String
(&) = (.)

-- wprowadzone typy END

-- inicjaca gracza
initialDirection :: Direction
initialDirection = U
-- inicjacja gracza END


arrowChars :: [Char]
arrowChars = ['A', 'B', 'C', 'D']

mapChars :: Char -> Char
mapChars oldChar
  | oldChar == 'A' = 'W'
  | oldChar == 'B' = 'S'
  | oldChar == 'C' = 'D'
  | otherwise = 'A'

newPair :: (Integer, Char) -> (Integer, Char)
newPair (oldVal, oldChar)
  | oldVal == 0 && oldChar == '\ESC' = (oldVal+1, oldChar)
  | oldVal == 1 && oldChar == '[' = (oldVal+1, oldChar)
  | oldVal == 2 && elemList oldChar arrowChars = (0, mapChars oldChar)
  | otherwise = (0, oldChar)

-- wszystkie aktywności
runActivity :: Activity world -> IO()
runActivity (Activity state handle draw) = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    putStr "\ESCc\ESC[?25l"
    putStr (draw state) 
    go (0, state) where
        go (oldVal, oldState) = do
            input <- getChar
            let (newVal, input') = newPair (oldVal, input)
            let state' = if newVal == 0
                         then handle (KeyPress [input']) oldState
                         else oldState
            putStr "\ESCc\ESC[?25l"
            putStr (draw state')
            go (newVal, state')

-- same litery
-- runActivity :: Activity world -> IO()
-- runActivity (Activity state handle draw) = do
--     hSetBuffering stdin NoBuffering
--     hSetBuffering stdout NoBuffering
--     putStr "\ESCc\ESC[?25l"
--     putStr $ (draw state) 
--     go state where
--         go oldState = do
--             input <- getChar
--             let state' = handle (KeyPress [input]) oldState
--             putStr "\ESCc\ESC[?25l"
--             putStr (draw state')
--             go state'

startScreen :: Screen
startScreen = "Sokoban!\n" ++ (intercalate "\ESC[0m " levels) ++ "\ESC[0m "

winScreen :: State -> Screen
winScreen state = res where 
  output = "Poziom ukończony, liczba ruchów:" ++ show (numOfMoves state)
  additional = if levelNumber state == numberOfMazes then "\nKoniec Gry" else ""
  res = output ++ additional

withStartScreen :: Eq s => Activity s -> Activity (Level s)
withStartScreen (Activity state handle draw)
  = Activity state' handle' draw'
  where
    state' = StartScreen

    handle' (KeyPress key) StartScreen
         | key == " "                  = Running state
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)

    draw' StartScreen = startScreen
    draw' (Running s) = draw s


resettable :: Activity s -> Activity s
resettable (Activity state handle draw)
  = Activity state handle' draw
  where handle' (KeyPress key) _ | key == "R" || key == "r" = state
        handle' e s = handle e s

withUndo :: Eq s => Activity s -> Activity (WithUndo s)
withUndo (Activity s handle draw) = Activity s' handle' draw' where
    s' = WithUndo s []
    handle' (KeyPress key) (WithUndo s stack) | key == "U" || key == "u"
      = case stack of s':stack' -> WithUndo s' stack'
                      []        -> WithUndo s []
    handle' e              (WithUndo s stack)
       | s' == s = WithUndo s stack
       | otherwise = WithUndo (handle e s) (s:stack)
      where s' = handle e s
    draw' (WithUndo s _) = draw s
    
handleEvent :: Event -> State -> State
handleEvent (KeyPress key) s
    | key == " " = if isWinning s then 
                      if levelNumber s < numberOfMazes 
                        then s{ playerCoord = startCoord (nth mazes (levelNumber s + 1)), 
                                boxCoords = (getBoxCoords (nth mazes (levelNumber s + 1))),
                                levelNumber = (levelNumber s + 1),
                                numOfMoves = 0}
                        else s
                   else s
    | isWinning s = s
    | key == "N" || key == "n" = if levelNumber s < numberOfMazes 
                      then s{ playerCoord = startCoord (nth mazes (levelNumber s + 1)), 
                              boxCoords = (getBoxCoords (nth mazes (levelNumber s + 1))),
                              levelNumber = (levelNumber s + 1),
                              numOfMoves = 0}
                      else s
    | key == "D" || key == "d" = makeMove R s
    | key == "W" || key == "w"   = makeMove U s
    | key == "A" || key == "a"  = makeMove L s
    | key == "S" || key == "s"  = makeMove D s
handleEvent _ s      = s    
-- wszystkie aktywności END

drawTile :: Tile -> String
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

removeBoxes :: (Coord -> Tile) -> Coord -> Tile
removeBoxes whichTile c 
  | whichTile c ==  Box = Ground
  | otherwise = whichTile c 

addBoxes :: [Coord] -> (Coord -> Tile) -> (Coord -> Tile)
addBoxes boxList whichTile c 
  | c `elem` boxList = Box
  | otherwise = whichTile c
  
findXSize :: Coord -> (Integer, Integer) -> (Integer, Integer)
findXSize coord acc 
  | coordX coord > fst acc = (coordX coord, snd acc)
  | coordX coord < snd acc = (fst acc, coordX coord)
  | otherwise = acc

findYSize :: Coord -> (Integer, Integer) -> (Integer, Integer)
findYSize coord acc 
  | coordY coord > fst acc = (coordY coord, snd acc)
  | coordY coord < snd acc = (fst acc, coordY coord)
  | otherwise = acc
     
checkMove :: Coord -> State -> Direction -> Bool
checkMove coord state dir = res where
  tileToMove = addBoxes (boxCoords state) (removeBoxes (getMazeDescription state)) coord
  tileForBoxMove = (addBoxes (boxCoords state) (removeBoxes (getMazeDescription state))) (adjacentCoord dir coord)
  res = if (tileToMove == Ground || tileToMove == Storage)
  then True
  else if (tileToMove == Box && (tileForBoxMove == Ground || tileForBoxMove == Storage))
       then True
       else False
        
makeMove :: Direction -> State -> State 
makeMove dir state = newState where
  newCoord = adjacentCoord dir (playerCoord state)
  res = checkMove newCoord state dir
  newState = (if res then state {playerCoord = newCoord, playerDir = dir, boxCoords = (moveBox (boxCoords state) dir newCoord), numOfMoves = (numOfMoves state + 1)} else state)
  
moveBox :: [Coord] -> Direction -> Coord -> [Coord]
moveBox (h:t) dir newCoord 
  | h == newCoord = (adjacentCoord dir h):t
  | otherwise = h:(moveBox t dir newCoord)
moveBox [] dir newCoord = []

getMazeDescription :: State -> (Coord -> Tile)
getMazeDescription state = coordToTile (nth mazes (levelNumber state))

isWinning :: State -> Bool
isWinning state = res where 
  whichTile = getMazeDescription state
  res = all (\x -> (whichTile x == Storage))(boxCoords state)

draw :: State -> Screen
draw s = if isWinning s then winScreen s else drawBoard s

drawCoord :: (Tile -> String) -> (Coord -> Tile) -> Coord -> String
drawCoord tileToStringFun coordToTileFun coord = tileToStringFun (coordToTileFun coord)

adjust :: Integer -> Integer -> DrawFun -> DrawFun
adjust movX movY fun (C x y) = fun (C (x+movX-1) (y+movY-1)) 

addPlayer :: Coord -> DrawFun -> DrawFun
addPlayer playerCoord fun (C x y)
    | coordX playerCoord == x && coordY playerCoord == y = player
    | otherwise = fun (C x y)

verticalFlip :: Integer -> DrawFun -> DrawFun
verticalFlip sizeY fun (C x y) = fun (C x (sizeY-y+1))

isBoxOnStorage :: [Coord] -> (Coord->Tile) -> DrawFun -> DrawFun
isBoxOnStorage boxList maze fun (C x y)
  | elemList (C x y) boxList && maze (C x y) == Storage = boxOnStorage
  | otherwise = fun (C x y)

isXTooLarge :: Integer -> DrawFun -> DrawFun
isXTooLarge valX fun (C x y)
  | x - 2 > valX = lastBlank
  | otherwise = fun (C x y)

drawBoard :: State -> Screen
drawBoard state = res where 
    coord = startCoord (nth mazes (levelNumber state))
    pCoord = playerCoord state
    maze = addBoxes (boxCoords state) (removeBoxes (getMazeDescription state))
    reachableVertices = ((allReachableVertices (reachableNeighbours (getMazeDescription state))) coord [])
    (maxX, minX) = foldList findXSize (coordX coord, coordX coord) reachableVertices
    (maxY, minY) = foldList findYSize (coordY coord, coordY coord) reachableVertices
    basicScreen = drawCoord drawTile maze
    modifyScreen = (verticalFlip (maxY-minY+1))&
                   (adjust minX minY)&
                   (isXTooLarge maxX)&
                   (isBoxOnStorage (boxCoords state) (getMazeDescription state))&
                   (addPlayer pCoord)
    res = (intercalate "" [if x<80 then (modifyScreen basicScreen) (C x y) else "\n" 
                            | y <- [0..(min 23 (maxY-minY+2))], x <- [0..80]]) ++ "\ESC[?25h"

--((verticalFlip (maxY-minY+1))((adjust minX minY)((addPlayer pCoord) basicScreen)))

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord U c = C (coordX c) ((coordY c) + 1)
adjacentCoord D c = C (coordX c) ((coordY c) - 1)
adjacentCoord L c = C ((coordX c) - 1) (coordY c)
adjacentCoord R c = C ((coordX c) + 1) (coordY c)

-- funkcje pomocnicze dla list 

-- testy funkcji
test1 = appendList listA listB
test2 = elemList elem1 listA
test3 = elemList elem1 listB
test4 = listLength listC
test5 = nth listC 5
test6 = mapList add_one listC
test7 = andList listBoolA
test8 = andList listBoolB
  
listA = [1, 2, 3]
listB = [4, 5, 6]
listC = [7, 8, 9, 0, 1, 2, 3, 4]
listBoolA = [True, False, True, True, True]
listBoolB = [True, True, True, True, True]
add_one x = x + 1
elem1 = 2
-- testy funkcji END

-- foldList
foldList :: (a -> b -> b) -> b -> [a] -> b
foldList fun acc (h:t) = fun h (foldList fun acc t)
foldList fun acc [] = acc
-- foldList END

-- appendList
addHead :: a -> [a] -> [a]
addHead h t = h:t

appendList :: [a] -> [a] -> [a]
appendList list1 list2 = foldList addHead list2 list1
-- appendList END

-- elemList
elemList :: Eq a => a -> [a] -> Bool
elemList element list = snd(foldList checkElem (element, False) list)

checkElem :: Eq a => a -> (a, Bool) -> (a, Bool)
checkElem new_elem (searched_elem, isSame)
  | isSame == True = (searched_elem, True)
  | new_elem == searched_elem = (searched_elem, True)
  | otherwise = (searched_elem, False)
-- elemList END

-- listLength
listLength :: [a] -> Integer
listLength list = foldList countElem 0 list

countElem :: a -> Integer -> Integer
countElem element acc = (acc + 1)
-- listLength END

-- nth 
nth :: [a] -> Integer -> a
nth list n = snd(foldList searchedElem ((listLength list - n), head list) list) 

searchedElem :: a -> (Integer, a) -> (Integer, a)
searchedElem element (n, old_elem)
  | n == 0 = ((n-1), element)
  | otherwise = ((n-1), old_elem)
-- nth END

-- mapList
mapList :: (a -> b) -> [a] -> [b]
mapList fun listA = foldList (mapElem fun) [] listA

mapElem :: (a -> b) -> a -> [b] -> [b]
mapElem fun element listB = (fun element):listB
-- mapList END

-- andList 
andList :: [Bool] -> Bool
andList boolList = foldList andElems True boolList

andElems :: Bool -> Bool -> Bool
andElems boolElem boolAcc = boolElem && boolAcc
-- andList END

-- allList 
allList :: (a -> Bool) -> [a] -> Bool
allList correctElem list = andList (mapList correctElem list)
-- andList END

-- funkcje pomocnicze dla list END

isGraphClosed :: Eq a => a -> (a -> [a]) -> (a -> Bool) -> Bool
isGraphClosed initial neighbours isOk 
  | not (isOk initial) = False
  | otherwise = allList isOk ((allReachableVertices neighbours) initial [])

allReachableVertices :: Eq a => (a -> [a]) -> a -> [a] -> [a]
allReachableVertices neighbours vertex acc
  | not(elemList vertex acc) = foldList (allReachableVertices neighbours) (vertex:acc) (neighbours vertex)
  | otherwise = acc

-- W przypadku gdy wejdę na pudełko zbieram tylko pudełko jako osiągalny i sąsiadów nie sprawdzam
-- Słyszałem od kolegi, że tak ma się zachowywać program.
reachableNeighbours :: (Coord -> Tile) -> Coord -> [Coord]
reachableNeighbours whichTile coord  = res where
  tile = whichTile coord
  res = if tile == Box then
    []
  else
    foldList (goodToMove whichTile) [] (adjacentCoords coord) 
    
goodToMove :: (Coord -> Tile) -> Coord -> [Coord] -> [Coord]
goodToMove whichTile coord acc 
  | elemList (whichTile coord) tilesToMove = coord:acc
  | otherwise = acc

adjacentCoords :: Coord -> [Coord]
adjacentCoords coord = mapList (\dir -> adjacentCoord dir coord) (dirList)

okVertex :: Maze -> Coord -> Bool 
okVertex testMaze coord 
  | elemList (coordToTile testMaze coord) tilesToMove = allList (\adjCoord -> not(coordToTile testMaze adjCoord == Blank))(adjacentCoords coord)
  | otherwise = False

reachable :: Eq a => a -> a -> (a -> [a]) -> Bool
reachable v initial neighbours = elemList v ((allReachableVertices neighbours) initial [])
  
allReachable :: Eq a => [a] -> a -> (a -> [a]) -> Bool 
allReachable vs initial neighbours = res where 
  reachableVertices = ((allReachableVertices neighbours) initial [])
  res = allList(\vertex -> elemList vertex reachableVertices)(vs)

isClosed :: Maze -> Bool 
isClosed testMaze = isGraphClosed (startCoord testMaze) (reachableNeighbours (coordToTile testMaze)) (okVertex testMaze)

absSubStoragesBoxes :: Tile -> Integer -> Integer 
absSubStoragesBoxes tile acc
  | tile == Box = (acc+1)
  | tile == Storage = (acc-1)
  | otherwise = acc

isSane :: Maze -> Bool
isSane testMaze = res where
  reachableVertices = ((allReachableVertices (reachableNeighbours (coordToTile testMaze))) (startCoord testMaze) [])
  reachableTiles = mapList (coordToTile testMaze) reachableVertices
  difference = foldList absSubStoragesBoxes 0 reachableTiles 
  res = if difference == 0 then True else False
  
isBoth :: Maze -> Bool
isBoth testMaze = isClosed testMaze && isSane testMaze

isBox :: (Coord -> Tile) -> Coord -> [Coord] -> [Coord] 
isBox whichTile coord list 
  | whichTile coord == Box = coord:list
  | otherwise = list

getBoxCoords :: Maze -> [Coord] 
getBoxCoords testMaze = res where
  reachableVertices = ((allReachableVertices (reachableNeighbours (coordToTile testMaze))) (startCoord testMaze) [])
  res = foldList (isBox (coordToTile testMaze)) [] reachableVertices 