module Main where
import SingleThreadedBlockChain
import Data.Time.Clock.System (getSystemTime)
import System.IO (readFile)

main :: IO ()
main = do
  args <- getArgs
  time <- show $ getSystemTime
  content <- readFile (args !! 0)
  let story = lines content
  putStr $ show $ makeBlockChain "" story 0 time
