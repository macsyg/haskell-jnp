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

drawTile :: Int -> Picture
drawTile x
    | x <= 0 = blank
    | x == 1 = wall
    | x == 2 = ground
    | x == 3 = storage
    | x == 4 = box
    | otherwise = blank
    
maze :: Int -> Int -> Int
maze x y
  | abs x > 4  || abs y > 4  = 0  -- blank
  | abs x == 4 || abs y == 4 = 1  -- wall
  | x ==  2 && y <= 0        = 1  -- wall
  | x ==  3 && y <= 0        = 3  -- storage
  | x >= -2 && y == 0        = 4  -- box
  | otherwise                = 2  -- ground
  
pictureOfMaze :: Picture

pictureOfMaze = pictures [translated (fromIntegral x) (fromIntegral y) (drawTile(maze x y)) 
                          | x <- [-10..10], y <- [-10..10]]