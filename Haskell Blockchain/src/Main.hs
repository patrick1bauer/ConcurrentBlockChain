module Main where
import SingleThreadedBlockChain
import Data.Time.Clock.System (getSystemTime)
import System.IO (readFile)
import System.TimeIt (timeIt)
import System.Environment ( getArgs )

main :: IO ()
main = do
  args <- getArgs
  time <- getSystemTime
  content <- readFile (args !! 0)
  let story = lines content
  timeIt $ putStrLn ("Blocks: " ++ (show (length (makeBlockChain "" story 0 (show time)))))

