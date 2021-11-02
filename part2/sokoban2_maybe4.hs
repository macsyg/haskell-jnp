import CodeWorld
main :: Program
main = program
type Program = IO ()

program = drawingOf(pictureOfMaze)

block = solidRectangle 1 1
border = rectangle 1 1

ground :: Picture
ground = border & colored (light green) block

wall :: Picture
wall = border & colored grey block

storage :: Picture
storage = border & scaled 0.5 0.5 wall & ground

box :: Picture
box = scaled 0.8 0.8 (pictures [translated (0+x) 0 rect | x <- [-0.4, -0.2, 0, 0.2, 0.4]] 
      & border 
      & colored brown block) & ground

rect = colored (dark brown) (solidRectangle 0.1 0.9)

player1 :: Picture
player1 = player_head 
          & (translated (-0.25) (0.1) player_hand) 
          & (translated (0.25) (0.1) player_hand)

player_head :: Picture
player_head = colored yellow (solidCircle 0.2)

player_hand :: Picture
player_hand = colored red (solidRectangle 0.1 0.4)

my_world :: State
my_world = S initialCoord initialDirection

-- typy BEGIN
data Tile = Wall | Ground | Storage | Box | Blank deriving (Eq)
data Direction = R | U | L | D
data Coord = C {
  coordX :: Integer,
  coordY :: Integer
}

data State = S {
  stCoord :: Coord,
  stDir :: Direction
}
-- typy END

-- inicjaca BEGIN
initialCoord :: Coord
initialCoord = C 1 (-1)

initialDirection :: Direction
initialDirection = U
-- inicjacja END

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

maze :: Integer -> Integer -> Tile
maze x y
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground
  
pictureOfMaze :: Picture
pictureOfMaze = pictures [translated (fromIntegral x) (fromIntegral y) (drawTile(maze x y)) 
                          | x <- [-10..10], y <- [-10..10]]
     
checkMove :: Coord -> Bool
checkMove c = goodTile (maze (coordX c) (coordY c))
    
goodTile :: Tile -> Bool
goodTile t 
  | t == Ground = True
  | t == Storage = True
  | otherwise = False
        
makeMove :: Direction -> State -> State
makeMove d s = new_s where
  new_c = adjacentCoord d (stCoord s)
  res = checkMove new_c
  new_s = (if res then s {stCoord = new_c, stDir = d} else s)

showDir :: Direction -> Double
showDir U = 0
showDir D = 180
showDir L = 90
showDir R = 270

drawState :: State -> Picture
drawState s = rotated (showDir (stDir s)) (atCoord (stCoord s) player1) & pictureOfMaze

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord U c = C (coordX c) ((coordY c) + 1)
adjacentCoord D c = C (coordX c) ((coordY c) - 1)
adjacentCoord L c = C ((coordX c) - 1) (coordY c)
adjacentCoord R c = C ((coordX c) + 1) (coordY c)
