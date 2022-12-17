module Main where

import Control.Monad
import Data.Char
import Data.List
import System.Environment
import System.IO
import System.IO.Unsafe

aux :: String -> ([Int], [Int]) -> ([Int], [Int])
aux [] (a, b) =
    ([],(sum a):b)
aux str (a, b) =
    ((read str) : a, b)

main = do
    contents <- getContents 
    let result = foldl (\acc x -> aux x acc) ([], []) (lines contents) 
        s = aux "" result 
        r =  sum $ take 3 (reverse (sort (snd s))) 
    print r
