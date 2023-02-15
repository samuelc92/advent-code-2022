import Data.Sequence
import Data.Foldable

isVisible :: Bool -> [Int] -> Int -> Int -> Bool
isVisible input l lIndex value =
    if input == False
        then False
    else if (l !! lIndex) >= value
        then False
    else True

isVisibleTop :: [[Int]] -> Int -> Int -> Int -> Bool 
isVisibleTop input value valueIndex index =
    foldl (\acc x -> isVisible acc x valueIndex value) True (Prelude.take index input) 

isVisibleBottom :: [[Int]] -> Int -> Int -> Int -> Bool 
isVisibleBottom input value valueIndex index =
    foldl (\acc x -> isVisible acc x valueIndex value) True (Prelude.drop (index + 1) input) 

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
