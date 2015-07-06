Module Wow where
import System.IO

parseFile fname = do
      handle <- openFile fname ReadMode
      contents <- hGetContents handle
      putStr contents
      hClose handle

main = do
      parseFile "main.wow"
      

