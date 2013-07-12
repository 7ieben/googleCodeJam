{-
Problem
Given a list of space separated words, reverse the order of the words. Each line
of text contains L letters and W words. A line will only consist of letters and
space characters. There will be exactly one space character between each pair of
consecutive words.

Input
The first line of input gives the number of cases, N.
N test cases follow. For each test case there will be a line of letters and space
characters indicating a list of space separated words. Spaces will not appear
at the start or end of a line.

Output
For each test case, output one line containing "Case #x: " followed by the
list of words in reverse order.
-}

import System.IO
import System.Environment
import System.Exit
import System.FilePath 
import Control.Monad (unless)

-- Takes the lineindex and the line and returnes the output for that line
-- called in process
solve :: Integer -> String -> String
solve n = (++) ("Case #" ++ show n ++ ": ") . unwords . reverse . words


-- processes the file representetd by infile by lines, calls solve, and writes
-- its output to outfile
-- called in main
process :: Handle -> Handle -> Integer -> IO ()
process infile outfile index = do
  inEOF <- hIsEOF infile
  unless inEOF $
         do 
           line <- hGetLine infile
           if length line == 1 then
               process infile outfile index
           else let outLine = solve index line in
                do
                  hPutStrLn outfile outLine
                  process infile outfile (index + 1)
                      
main :: IO ()
main = do
  args <- getArgs
  if takeExtension (head args) == ".in" then
    do infile <- openFile (head args) ReadMode
       outfile <- openFile (takeBaseName (head args) ++ ".out") WriteMode
       process infile outfile 1
       hClose infile
       hClose outfile
       putStrLn "Success!!!"
       exitSuccess
    else
    do prog <- getProgName
       hPutStrLn stderr $ "Use: " ++ prog ++ " InputFile.in"
       exitWith (ExitFailure 1)  
