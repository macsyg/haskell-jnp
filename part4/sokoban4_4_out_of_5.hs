{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
main :: Program
type Program = IO ()

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

main = drawingOf(pictureOfBools (mapList isBoth mazes))
--main = print difference

--reachableVertices = snd(allReachableVertices initialCoord (reachableNeighbours, []))
--reachableTiles = mapList maze reachableVertices
--difference = foldList absSubStoragesBoxes 0 reachableTiles

--main = print (isGraphClosed (startCoord firstMaze) reachableNeighbours (okVertex firstMaze))
--main = runActivity (resettable (withStartScreen (Activity my_world handleEvent draw)))

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
my_world :: State
my_world = S initialCoord initialDirection (getBoxCoords firstMaze)
-- stan gry END

-- wprowadzone typy
data Tile = Wall | Ground | Storage | Box | Blank deriving (Eq, Show)
data Direction = R | U | L | D

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
  boxCoords :: [Coord]
}

data Activity world = Activity {
  actState  :: world,
  actHandle :: (Event -> world -> world),
  actDraw   :: (world -> Picture)
}

data Level world = StartScreen | Running world

data Maze = M {
  startCoord :: Coord,
  coordToTile :: (Coord -> Tile)
}
-- wprowadzone typy END

-- inicjaca gracza
initialCoord :: Coord
initialCoord = C 1 (-1)

initialDirection :: Direction
initialDirection = U

--initialBoxes :: [Coord]
--initialBoxes = [(C (-2) 0), (C (-1) 0), (C 0 0), (C 1 0)]
-- inicjacja gracza END

-- straszne rzeczy

runActivity :: Activity world -> IO()
runActivity activity = activityOf (actState activity) (actHandle activity) (actDraw activity)

startScreen :: Picture
startScreen = scaled 3 3 (lettering "Sokoban!")

winScreen :: Picture
winScreen = scaled 3 3 (lettering "You Win!")

withStartScreen :: Activity s -> Activity (Level s)
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
-- straszne rzeczy END

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

maze :: Coord -> Tile
maze c
  | abs (coordX c) > 4  || abs (coordY c) > 4  = Blank
  | abs (coordX c) == 4 || abs (coordY c) == 4 = Wall
  | coordX c ==  2 && coordY c <= 0        = Wall
  | coordX c ==  3 && coordY c <= 0        = Storage
  | coordX c >= -2 && coordY c == 0        = Box
  | otherwise                = Ground
  
firstMaze = M initialCoord maze
mazes = [firstMaze]

removeBoxes :: (Coord -> Tile) -> Coord -> Tile
removeBoxes whichTile c 
  | whichTile c ==  Box = Ground
  | otherwise = whichTile c 

addBoxes :: [Coord] -> (Coord -> Tile) -> (Coord -> Tile)
addBoxes boxList whichTile c 
  | c `elem` boxList = Box
  | otherwise = whichTile c

pictureOfMaze :: State -> Picture 
pictureOfMaze s = pictures [translated (fromIntegral x) (fromIntegral y) (drawTile((addBoxes (boxCoords s) (removeBoxes (coordToTile (M initialCoord maze))) (C x y)) )) 
                          | x <- [-10..10], y <- [-10..10]]
     
checkMove :: Coord -> State -> Direction -> Bool
checkMove coord state dir = res where
  tileToMove = addBoxes (boxCoords state) (removeBoxes maze) coord
  tileForBoxMove = (addBoxes (boxCoords state) (removeBoxes maze)) (adjacentCoord dir coord)
  res = if (tileToMove == Ground || tileToMove == Storage)
  then True
  else if (tileToMove == Box && (tileForBoxMove == Ground || tileForBoxMove == Storage))
       then True
       else False
        
makeMove :: Direction -> State -> State
makeMove dir state = newState where
  newCoord = adjacentCoord dir (playerCoord state)
  res = checkMove newCoord state dir
  newState = (if res then state {playerCoord = newCoord, playerDir = dir, boxCoords = (moveBox (boxCoords state) dir newCoord)} else state)
  
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

isWinning :: State -> Bool
isWinning state = all (\x -> (maze x == Storage))(boxCoords state)

draw :: State -> Picture
draw s = if isWinning s then winScreen else (atCoord (playerCoord s) (player2 (playerDir s))) & pictureOfMaze s

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord U c = C (coordX c) ((coordY c) + 1)
adjacentCoord D c = C (coordX c) ((coordY c) - 1)
adjacentCoord L c = C ((coordX c) - 1) (coordY c)
adjacentCoord R c = C ((coordX c) + 1) (coordY c)

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) s
    | key == "Right" = makeMove R s
    | key == "Up"    = makeMove U s
    | key == "Left"  = makeMove L s
    | key == "Down"  = makeMove D s
handleEvent _ s      = s


-- funkcje pomocnicze dla list 
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

foldList :: (a -> b -> b) -> b -> [a] -> b
foldList fun acc (h:t) = fun h (foldList fun acc t)
foldList fun acc [] = acc

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
mapList fun listA = snd(foldList mapElem (fun,[]) listA)

mapElem :: a -> ((a -> b), [b]) -> ((a -> b), [b])
mapElem element (fun, listB) = (fun, (fun element):listB)
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

third :: (a, b, c) -> c
third (_, _, x) = x

isGraphClosed :: Eq a => a -> (a -> [a]) -> (a -> Bool) -> Bool
isGraphClosed initial neighbours isOk 
  | not (isOk initial) = False
  | otherwise = allList isOk (snd (allReachableVertices initial (neighbours, [])))

allReachableVertices :: Eq a => a -> ((a -> [a]), [a]) -> ((a -> [a]), [a])
allReachableVertices vertex (neighbours, acc)
  | not(elemList vertex acc) = foldList allReachableVertices (neighbours, vertex:acc) (neighbours vertex)
  | otherwise = (neighbours, acc)

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
reachable v initial neighbours = elemList v (snd (allReachableVertices initial (neighbours, [])))
  
allReachable :: Eq a => [a] -> a -> (a -> [a]) -> Bool
allReachable vs initial neighbours = res where 
  reachableVertices = (snd (allReachableVertices initial (neighbours, [])))
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
  reachableVertices = snd(allReachableVertices (startCoord testMaze) ((reachableNeighbours (coordToTile testMaze)), []))
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
  reachableVertices = snd(allReachableVertices (startCoord testMaze) ((reachableNeighbours (coordToTile testMaze)), []))
  res = foldList (isBox (coordToTile testMaze)) [] reachableVertices 
  









