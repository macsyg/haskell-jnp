import CodeWorld
main :: Program
main = program
type Program = IO ()

activityOf :: world ->
              (Event -> world -> world) ->
              (world -> Picture) ->
              IO ()


program = drawingOf(pictureOfMaze)

world :: State
world = S initialCoord initialDirection pictureOfMaze

-- grafiki START
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
          & colored (light green) (solidRectangle 1 1)

player_head :: Picture
player_head = colored yellow (solidCircle 0.2)

player_hand :: Picture
player_hand = colored red (solidRectangle 0.1 0.4)
-- grafiki END

-- typy START
data Tile = Wall | Ground | Storage | Box | Blank
data Direction = R | U | L | D
data Coord = C Integer Integer

data State = S {
  stPlayer :: Coord,
  stDir :: Direction,
  mazeMap :: Picture
}
-- typy END

-- inicjacja gracza START
initialCoord :: Coord
initialCoord = C 1 (-1)

initialDirection :: Direction
initialDirection = U
-- inicjacja gracza END

-- funkcje START
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
                          | x <- [-10..10], y <- [-10..10]
-- funkcje END