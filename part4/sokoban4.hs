{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import CodeWorld
main :: Program
type Program = IO ()

main = etap5

pictureOfBools :: [Bool] -> Picture
pictureOfBools xs = translated (-fromIntegral k / 2) (fromIntegral k) (go 0 xs)
  where n = length xs
        k = findK 0 -- k is the integer square of n
        findK i | i * i >= n = i
                | otherwise  = findK (i+1)
        go _ [] = blank
        go i (b:bs) =
          translated (fromIntegral (i `mod` k))
                     (-fromIntegral (i `div` k))
                     (pictureOfBool b)
          & go (i+1) bs

        pictureOfBool True =  colored green (solidCircle 0.4)
        pictureOfBool False = colored red   (solidCircle 0.4)

etap4 :: Picture
etap4 = pictureOfBools (mapList isBoth (appendList mazes badMazes))

etap5 :: IO()
etap5 = runActivity (resettable (withUndo (withStartScreen (Activity initialState handleEvent draw))))

-- testy programu
--test = print (isGraphClosed (startCoord firstMaze) reachableNeighbours (okVertex firstMaze))
--test = print (allReachableVertices (reachableNeighbours (coordToTile fourthMaze)) (startCoord fourthMaze) [])
--test = print (isSane fourthMaze)
-- testy programu END

-- bloki
block, border, rect :: Picture
block = solidRectangle 1 1
border = rectangle 1 1
rect = colored (dark brown) (solidRectangle 0.1 0.9)

ground, wall, storage, box :: Picture
ground = border & colored (light green) block
wall = border & colored grey block
storage = border & scaled 0.5 0.5 wall & ground
box = scaled 0.8 0.8 (pictures [translated (0+x) 0 rect | x <- [-0.4, -0.2, 0, 0.2, 0.4]] 
      & border 
      & colored brown block) & ground
-- bloki END

-- gracz
player1 :: Picture
player2 :: Direction -> Picture
player_head, player_hand :: Picture

player_head = colored yellow (solidCircle 0.2)
player_hand = colored red (solidRectangle 0.1 0.4)

player1 = player_head 
          & (translated (-0.25) (0.1) player_hand) 
          & (translated (0.25) (0.1) player_hand)

player2 dir = rotated (showDir dir) player1 
-- gracz END

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
  actDraw   :: (world -> Picture)
} 

data Level world = StartScreen | Running world deriving Eq

data Maze = Maze {
  startCoord :: Coord,
  coordToTile :: (Coord -> Tile)
}

data WithUndo a = WithUndo a [a]
-- wprowadzone typy END

-- inicjaca gracza
initialDirection :: Direction
initialDirection = U
-- inicjacja gracza END

-- wszystkie aktywności
runActivity :: Activity world -> IO()
runActivity activity = activityOf (actState activity) (actHandle activity) (actDraw activity)

startScreen :: Picture
startScreen = scaled 2 2 (lettering "Sokoban!") & translated 0 (-4) etap4

winScreen :: State -> Picture
winScreen state = res where 
  output = scaled 1 1 (lettering ("Poziom ukończony, liczba ruchów:" <> (T.pack (show (numOfMoves state)))) )
  additional = if levelNumber state == numberOfMazes then translated (0) (-2) (scaled 1 1 (lettering ("Koniec Gry"))) else blank
  res = output & additional

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
  where handle' (KeyPress key) _ | key == "Esc" = state
        handle' e s = handle e s

withUndo :: Eq s => Activity s -> Activity (WithUndo s)
withUndo (Activity s handle draw) = Activity s' handle' draw' where
    s' = WithUndo s []
    handle' (KeyPress key) (WithUndo s stack) | key == "U"
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
    | key == "N" = if levelNumber s < numberOfMazes 
                      then s{ playerCoord = startCoord (nth mazes (levelNumber s + 1)), 
                              boxCoords = (getBoxCoords (nth mazes (levelNumber s + 1))),
                              levelNumber = (levelNumber s + 1),
                              numOfMoves = 0}
                      else s
    | key == "Right" = makeMove R s
    | key == "Up"    = makeMove U s
    | key == "Left"  = makeMove L s
    | key == "Down"  = makeMove D s
handleEvent _ s      = s    
-- wszystkie aktywności END

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

drawTile :: Tile -> Picture
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
  
  
pictureOfMaze :: State -> Picture
pictureOfMaze state = res where 
  coord = startCoord (nth mazes (levelNumber state))
  reachableVertices = ((allReachableVertices (reachableNeighbours (getMazeDescription state))) coord [])
  (maxX, minX) = foldList findXSize (coordX coord, coordX coord) reachableVertices
  (maxY, minY) = foldList findYSize (coordY coord, coordY coord) reachableVertices
  res = pictures [translated (fromIntegral x) (fromIntegral y) (drawTile((addBoxes (boxCoords state) (removeBoxes (getMazeDescription state)) (C x y)) )) 
                          | x <- [(minX-1)..(maxX+1)], y <- [(minY-1)..(maxY+1)]]  
  
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

showDir :: Direction -> Double
showDir U = 0
showDir D = pi
showDir L = pi/2
showDir R = 3*pi/2

getMazeDescription :: State -> (Coord -> Tile)
getMazeDescription state = coordToTile (nth mazes (levelNumber state))

isWinning :: State -> Bool
isWinning state = res where 
  whichTile = getMazeDescription state
  res = all (\x -> (whichTile x == Storage))(boxCoords state)

draw :: State -> Picture
draw s = if isWinning s then winScreen s else (atCoord (playerCoord s) (player2 (playerDir s))) & pictureOfMaze s

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