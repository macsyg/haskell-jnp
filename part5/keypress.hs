import System.IO

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  x <- getChar
  putStrLn ("You pressed: " ++ [x])