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
import Data.List 

reverseWords :: Integer -> String -> String
reverseWords n = ((++) ("Case #" ++ show n ++ " ")) . unwords . reverse . words

solve :: Handle -> Handle -> Integer -> IO ()
solve infile outfile index = do
  inEOF <- hIsEOF infile
  if inEOF
  then return ()
  else do 
   line <- hGetLine infile
   if (length line == 1) then
       do
         solve infile outfile index
   else do 
     outLine <- return(reverseWords index line)
     hPutStrLn outfile outLine
     solve infile outfile (index + 1)
                      
main :: IO ()
main = do
  args <- getArgs
  case args of
    (x:xs) -> do
              if (takeExtension x == ".in") then
                do
                  infile  <- openFile x ReadMode  
                  outfile <- openFile "OutFile.out" WriteMode
                  solve infile outfile 1
                  hClose infile
                  hClose outfile
                  putStrLn "Success!!!"
                  exitWith ExitSuccess
                else do 
                  prog <- getProgName
                  hPutStrLn stderr ("Use: InputFile . in")
                  exitWith (ExitFailure 1)
    _ -> do 
         prog <- getProgName
         hPutStrLn stderr ("Use: " ++ prog ++ "Inputfile")
         exitWith (ExitFailure 2)
    