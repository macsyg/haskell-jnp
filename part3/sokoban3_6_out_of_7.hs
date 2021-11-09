{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
main :: Program
type Program = IO ()

main = runActivity (resettable (withStartScreen (Activity (S initialCoord initialDirection initialBoxes) handleEvent draw)))

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
my_world = S initialCoord initialDirection initialBoxes
-- stan gry END

-- wprowadzone typy
data Tile = Wall | Ground | Storage | Box | Blank deriving (Eq)
data Direction = R | U | L | D
data Coord = C {
  coordX :: Integer,
  coordY :: Integer
} deriving (Eq)
data State = S {
  playerCoord :: Coord,
  playerDir :: Direction,
  boxCoords :: [Coord]
}

data Activity world = Activity {
  actState  :: world,
  actHandle :: (Event -> world -> world),
  actDraw   ::(world -> Picture)
}

data Level world = StartScreen | Running world

type Maze = Coord -> Tile
-- wprowadzone typy END

-- inicjaca gracza
initialCoord :: Coord
initialCoord = C 1 (-1)

initialDirection :: Direction
initialDirection = U

initialBoxes :: [Coord]
initialBoxes = [(C (-2) 0), (C (-1) 0), (C 0 0), (C 1 0)]
-- inicjacja gracza END

-- straszne rzeczy

runActivity :: Activity world -> IO()
runActivity activity = activityOf (actState activity) (actHandle activity) (actDraw activity)

startScreen :: Picture
startScreen = scaled 3 3 (lettering "Sokoban!")

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

removeBoxes :: Maze -> Coord -> Tile
removeBoxes maze c 
  | maze c ==  Box = Ground
  | otherwise = maze c 

addBoxes :: [Coord] -> Maze -> Maze
addBoxes boxList maze c 
  | c `elem` boxList = Box
  | otherwise = maze c

pictureOfMaze :: State -> Picture 
pictureOfMaze s = pictures [translated (fromIntegral x) (fromIntegral y) (drawTile((addBoxes (boxCoords s) (removeBoxes maze) (C x y)) )) 
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

draw :: State -> Picture
draw s = (atCoord (playerCoord s) (player2 (playerDir s))) & pictureOfMaze s

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

-- Po puszczeniu klawisza "Esc" nic się nie dzieje.
reset :: (Event -> world -> world) -> 
          world ->
          (Event -> world -> world)    
reset handleEvent initialWorld (KeyPress key) resettableWorld
     | key == "Esc" = initialWorld
     | otherwise = handleEvent (KeyPress key) resettableWorld
reset handleEvent initialWorld _ resettableWorld = resettableWorld

resettableActivityOf ::
    world ->
    (Event -> world -> world) ->
    (world -> Picture) ->
    IO ()
resettableActivityOf world handleEvent drawState = activityOf world (reset handleEvent world) drawState