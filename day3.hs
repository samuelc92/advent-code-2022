module Main where

import Control.Monad
import Data.Char
import Data.List
import qualified Data.Map as Map
import System.Environment
import System.IO
import System.IO.Unsafe

repeatsLetters :: String -> [Char]
repeatsLetters a =
        let size         = fromIntegral $ length a
            halfSize     = size `div` 2
            splitted     = splitAt halfSize a
            itensZipped  = zip (fst splitted) (take halfSize $ repeat True)
            itensMap     = Map.fromList itensZipped
        in nub $ filter (\x->Map.member x (Map.fromList itensZipped)) (snd splitted)  

repeatsLetters2 :: [String] -> String
repeatsLetters2 l =
        nub $ foldl intersect (head l) (tail l)

process :: [String] -> [String] -> Int -> Int
process [] lInitial acc = acc
process lAtual lInitial acc =
        let x = head $ repeatsLetters2 lAtual
            r = if (isUpper x) then (ord x) - 38 else (ord x) - 96
        in process (take 3 lInitial) (drop 3 lInitial) acc + r 


main = do
    contents <- getContents
    --let result = foldl (\acc x-> acc + (sum $ map (\y->if (isUpper y) then (ord y) - 38 else (ord y) - 96) (repeatsLetters x))) 0 (lines contents)
    let l = lines contents
        result = process (take 3 l) (drop 3 l) 0
    print result
