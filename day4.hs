module Main where

import Control.Monad
import Data.Char
import Data.List
import System.Environment
import System.IO
import System.IO.Unsafe

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

isOverlaping :: (Int, Int) -> (Int, Int) -> Int 
isOverlaping (x1, y1) (x2, y2) =
    if ((x1 <= x2) && (y1 >= y2)) || ((x1 >= x2) && (x1 <= y2) && (y1 <= y2))
        then 1
        else 0

processing :: String -> Int
processing s =
    let a  = wordsWhen (== ',') s
        b  = wordsWhen (== '-') (head a)
        c  = wordsWhen (== '-') (last a)
        t1 = (read (head b) :: Int, read (last b) :: Int)
        t2 = (read (head c) :: Int, read (last c) :: Int)
    in isOverlaping t1 t2

main = do
    contents <- getContents
    let r = foldl (\acc x-> acc + (processing x)) 0 (lines contents)
    print r
