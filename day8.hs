import Data.Sequence
import Data.Foldable

isVisible :: Bool -> [Int] -> Int -> Int -> Bool
isVisible input l lIndex value =
    if input == False
        then False
    else if (l !! lIndex) >= value
        then False
    else True

isVisibleLeft :: [Int] -> Int -> Int -> Bool 
isVisibleLeft input value valueIndex =
    foldl (\acc x -> if acc == False
                        then False
                    else if x >= value
                            then False
                    else True) True (Prelude.take valueIndex input) 

isVisibleRight :: [Int] -> Int -> Int -> Bool 
isVisibleRight input value valueIndex =
    foldl (\acc x -> if acc == False
                        then False
                    else if x >= value
                            then False
                    else True) True (Prelude.drop (valueIndex + 1) input) 

isVisibleTop :: [[Int]] -> Int -> Int -> Int -> Bool 
isVisibleTop input value valueIndex index =
    foldl (\acc x -> isVisible acc x valueIndex value) True (Prelude.take index input) 

isVisibleBottom :: [[Int]] -> Int -> Int -> Int -> Bool 
isVisibleBottom input value valueIndex index =
    foldl (\acc x -> isVisible acc x valueIndex value) True (Prelude.drop (index + 1) input) 

process :: [[Int]] -> [Int] -> Int -> Int
process input l index =
    sum $ toList $ mapWithIndex(\i v -> if (isVisibleTop input v i index) || (isVisibleBottom input v i index) || (isVisibleLeft l v i) ||(isVisibleRight l v i) then 1 else 0) (fromList l)

run :: [[Int]] -> [Int]
run l =
    toList $ mapWithIndex(\i v -> if i == 0
                then Prelude.length v
                else if i == (Prelude.length l) - 1
                    then Prelude.length v
                    else (process l v i)) (fromList l)
