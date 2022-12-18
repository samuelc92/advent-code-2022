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

main = do
    contents <- getContents
    let result = foldl (\acc x-> acc + (sum $ map (\y->if (isUpper y) then (ord y) - 38 else (ord y) - 96) (repeatsLetters x))) 0 (lines contents)
    print result
