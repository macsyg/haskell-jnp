import System.IO

doesQuit :: Char -> Bool
doesQuit 'q' = True
doesQuit _ = False

-- promptLine :: String -> IO String
-- promptLine prompt = do
--     putStr prompt
--     getLine

main = mainLoop
mainLoop :: IO()
mainLoop = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering  
  input <- getChar
  putStr "\ESCc"
  if doesQuit input
     then return ()
     else processInput [input] >> mainLoop

processInput :: String -> IO ()
processInput input =
  putStrLn $ "Powiedziałeś: " ++ input