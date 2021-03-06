{-
Problem
You receive a credit C at a local store and would like to buy two items. 
You first walk through the store and create a list L of all available items. 
From this list you would like to buy two items that add up to the entire value of the credit. 
The solution you provide will consist of the two integers indicating the positions of the items 
in your list (smaller number first).

Input
The first line of input gives the number of cases, N. N test cases follow. For each test case there will be:

One line containing the value C, the amount of credit you have at the store.
One line containing the value I, the number of items in the store.
One line containing a space separated list of I integers. Each integer P indicates the price of an item in the store.

Each test case will have exactly one solution.

Output
For each test case, output one line containing "Case #x: " followed by the indices of the two items whose price 
adds up to the store credit. The lower index should be output first.
-}

import System.IO
import System.Environment
import System.Exit
import System.FilePath
import Data.List
import Control.Monad (unless)

indexOf :: (Int,Int) -> [Int] -> (Int,Int)
indexOf (x,y) intList = (xindex,yindex)
    where xindex = 1 + head (elemIndices x intList)
          yindex = length intList - head (elemIndices y $ reverse intList)

solve :: Int -> [Int] -> Int -> String
solve n intList case' = "Case #" ++ show case' ++ ": " ++ 
                        show (fst itemindex) ++ " " ++ show (snd itemindex)
    where items = head [(x,y) | x <- intList, y <- intList, x + y == n]
          itemindex = indexOf items intList

process :: Handle -> Handle -> Int -> IO ()
process infile outfile index = do
  inEOF <- hIsEOF infile
  unless inEOF $
         do
           hGetLine infile
           line2 <- hGetLine infile
           line3 <- hGetLine infile
           let n = (read line2 :: Int)
               list = (map read (words line3) :: [Int]) in
                     do 
                       hPutStrLn outfile $ solve n list index
                       process infile outfile $ index + 1

main :: IO ()
main = do
  args <- getArgs
  case args of
    (x:_) -> if takeExtension x == ".in" then
                do
                  infile  <- openFile x ReadMode  
                  outfile <- openFile (takeBaseName x ++ ".out") WriteMode
                  process infile outfile 1
                  hClose infile
                  hClose outfile
                  putStrLn "Success!!!"
                  exitSuccess
                else do 
                  prog <- getProgName
                  hPutStrLn stderr $ "Use: " ++ prog ++ " InputFile . in"
                  exitWith (ExitFailure 1)
    _ -> do 
         prog <- getProgName
         hPutStrLn stderr ("Use: " ++ prog ++ "Inputfile")
         exitWith (ExitFailure 2)
    
