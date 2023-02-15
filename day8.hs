import Data.Sequence
import Data.Foldable

isVisibleTop :: [[Int]] -> Int -> Int -> Int -> Bool 
isVisibleTop input value valueIndex index =
    foldl (\acc x -> if acc == False
                        then False
                    else if (x !! valueIndex) >= value
                        then False
                        else True) True (Prelude.take index input) 

process :: [[Int]] -> [Int] -> Int -> Int
process input l index =
    sum $ toList $ mapWithIndex(\i v -> if (isVisibleTop input v i index) then 1 else 0) (fromList l)

run :: [[Int]] -> [Int]
run l =
    toList $ mapWithIndex(\i v -> if i == 0
                then 0
                else if i == (Prelude.length l) - 1
                    then 0
                    else (process l v i)) (fromList l)
