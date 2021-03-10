module Main where
import SingleThreadedBlockChain

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (args !! 0)
  putStr $ show $ makeBlockChain "" story 0
     where
       story = lines content
