module Main where

import Control.Monad
import Data.Char
import Data.List
import System.Environment
import System.IO
import System.IO.Unsafe

-- A=X=ROCK=1
-- B=Y=PAPER=2
-- C=Z=SCISSORS=3
--
-- X = lose=0
-- Y = draw=3
-- Z = win=6

points :: String -> String -> Int
points "X" "A" = 3+0
points "X" "B" = 1+0
points "X" "C" = 2+0
points "Y" "A" = 1+3
points "Y" "B" = 2+3
points "Y" "C" = 3+3
points "Z" "A" = 2+6
points "Z" "B" = 3+6
points "Z" "C" = 1+6
points _ _ = 0

processInput :: String -> Int
processInput i =
    let strArr = words i
        f      = head strArr
        s      = last strArr
    in points s f

main = do
    contents <- getContents
    --forM (lines contents)
    let result = foldl(\acc x-> acc + (processInput x)) 0 (lines contents) 
    --mapM_ (\x-> print $ words x) (lines contents)
    print $ result

