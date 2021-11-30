import System.IO

promptLine :: String -> IO String
promptLine prompt = do
    putStr prompt
    getLine

main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  x <- getChar
  putStr "\ESCc"
  putStrLn $ "Powiedziałeś: " ++ [x]