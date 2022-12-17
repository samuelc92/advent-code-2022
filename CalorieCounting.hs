module Main where

import Control.Monad
import Data.Char
import System.Environment
import System.IO
import System.IO.Unsafe

aux :: String -> ([Int], Int) -> ([Int], Int)
aux [] (a, b) =
    let s = sum a
    in if s > b
           then ([], s)
           else ([], b)
aux str (a, b) =
    ((read str) : a, b)

main = do
    contents <- getContents 
    let result = foldl (\acc x -> aux x acc) ([], 0) (lines contents) 
    print $ aux "" result
