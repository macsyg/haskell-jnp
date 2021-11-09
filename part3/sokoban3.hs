{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
main :: Program
type Program = IO ()

main = walk3

walk3 :: IO()
walk3 = resettableActivityOf my_world handleEvent drawState2

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
  stCoord :: Coord,
  stDir :: Direction,
  boxCoords :: [Coord] 
}

type Maze = Coord -> Tile
-- wprowadzone typy END

-- inicjaca gracza
initialCoord :: Coord
initialCoord = C 1 (-1)

initialDirection :: Direction
initialDirection = U

initialBoxes :: [Coord]
initialBoxes = [(C 0 (-2)), (C 0 (-1)), (C 0 0), (C 0 1)]
-- inicjacja gracza END

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
  | maze c == Box = Ground

addBoxes :: [Coord] -> Maze -> Maze
addBoxes boxList maze c 
  | c `elem` boxList = Box
  | otherwise = maze c

pictureOfMaze :: State -> Picture 
pictureOfMaze s = pictures [translated (fromIntegral x) (fromIntegral y) (drawTile((addBoxes (boxCoords s) (removeBoxes maze) (C x y)) )) 
                          | x <- [-10..10], y <- [-10..10]]
     
checkMove :: Coord -> Bool
checkMove c = goodTile (removeBoxes maze c)
    
goodTile :: Tile -> Bool
goodTile t 
  | t == Ground = True
  | t == Storage = True
  | otherwise = False
        
makeMove :: Direction -> State -> State
makeMove dir state = newState where
  newCoord = adjacentCoord dir (stCoord state)
  res = checkMove newCoord
  newState = (if res then state {stCoord = newCoord, stDir = dir} else state)

showDir :: Direction -> Double
showDir U = 0
showDir D = pi
showDir L = pi/2
showDir R = 3*pi/2

drawState2 :: State -> Picture
drawState2 s = (atCoord (stCoord s) (player2 (stDir s))) & pictureOfMaze

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