import Data.Sequence
import Data.Foldable
import Data.Char

countVisibleLeftTree :: [Int] -> Int -> Int -> Int 
countVisibleLeftTree input value valueIndex =
    let c = Prelude.length $ takeWhile (< value) (Prelude.reverse (Prelude.take valueIndex input))
    in if c < (Prelude.length $ Prelude.take valueIndex input)
           then c + 1
           else c + 0

countVisibleRightTree :: [Int] -> Int -> Int -> Int 
countVisibleRightTree input value valueIndex =
    let c = Prelude.length $ takeWhile (< value) (Prelude.drop (valueIndex + 1) input)
    in if c < (Prelude.length $ Prelude.drop (valueIndex + 1) input)
           then c + 1
           else c + 0

countVisibleTopTree :: [[Int]] -> Int -> Int -> Int -> Int 
countVisibleTopTree input value valueIndex index =
    let c = Prelude.length $ takeWhile (\l -> (l !! valueIndex) < value) (Prelude.reverse $ Prelude.take index input)
    in if c < (Prelude.length $ Prelude.take index input)
           then c + 1
           else c + 0

countVisibleBottomTree :: [[Int]] -> Int -> Int -> Int -> Int 
countVisibleBottomTree input value valueIndex index =
    let c = Prelude.length $ takeWhile (\l -> (l !! valueIndex) < value) (Prelude.drop (index + 1) input)
    in if c < (Prelude.length $ Prelude.drop (index + 1) input)
           then c + 1
           else c + 0

process :: [[Int]] -> [Int] -> Int -> Int
process input l index =
    maximum $ toList $ mapWithIndex(\i v -> (countVisibleTopTree input v i index) * (countVisibleBottomTree input v i index) * (countVisibleLeftTree l v i) * (countVisibleRightTree l v i)) (fromList l)

run :: [[Int]] -> [Int]
run l =
    toList $ mapWithIndex(\i v -> if i == 0
                then Prelude.length v
                else if i == (Prelude.length l) - 1
                    then Prelude.length v
                    else (process l v i)) (fromList l)

main = do
    contents <- getContents
    let l = foldr (\x acc -> (map digitToInt x) : acc) [] (lines contents)
    print $ maximum $ run l
