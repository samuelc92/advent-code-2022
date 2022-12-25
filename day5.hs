import Data.Char
import Data.Sequence
import Data.Foldable

infixr 5 :-:
data Stack = EmptyS | Char :-: Stack deriving (Show, Read, Eq, Ord)

push :: Stack -> Char -> Stack
push EmptyS a  = a :-: EmptyS 
push s a = a :-: s 

pop :: Stack -> (Char, Stack)
pop EmptyS = (' ', EmptyS)
pop (x:-:xs) = (x, xs)


buildStack :: [Stack]
buildStack =
    [
         'Q' :-: 'F' :-: 'L' :-: 'S' :-: 'R' :-: EmptyS,
         'T' :-: 'P' :-: 'G' :-: 'Q' :-: 'Z' :-: 'N' :-: EmptyS,
         'B' :-: 'Q' :-: 'M' :-: 'S' :-: EmptyS,
         'Q' :-: 'B' :-: 'C' :-: 'H' :-: 'J' :-: 'Z' :-: 'G' :-: 'T' :-: EmptyS,
         'S' :-: 'F' :-: 'N' :-: 'B' :-: 'M' :-: 'H' :-: 'P' :-: EmptyS,
         'G' :-: 'V' :-: 'L' :-: 'S' :-: 'N' :-: 'Q' :-: 'C' :-: 'P' :-: EmptyS,
         'F' :-: 'C' :-: 'W' :-: EmptyS,
         'M' :-: 'P' :-: 'V' :-: 'W' :-: 'Z' :-: 'G' :-: 'H' :-: 'Q' :-: EmptyS,
         'R' :-: 'N' :-: 'C' :-: 'L' :-: 'D' :-: 'Z' :-: 'G' :-: EmptyS
    ]

buildStackT :: [Stack]
buildStackT =
    [
         'N' :-: 'Z' :-: EmptyS,
         'D' :-: 'C' :-: 'M' :-: EmptyS,
         'P' :-: EmptyS
    ]

type Stack2 = [[Char]]
buildStack2 :: Stack2
buildStack2 =
    [
         ['Q', 'F', 'L', 'S', 'R'],
         ['T','P', 'G', 'Q', 'Z', 'N'],
         ['B', 'Q', 'M', 'S'],
         ['Q', 'B', 'C', 'H', 'J', 'Z', 'G', 'T'],
         ['S', 'F', 'N', 'B', 'M', 'H', 'P'],
         ['G', 'V', 'L', 'S', 'N', 'Q', 'C', 'P'],
         ['F', 'C', 'W'],
         ['M', 'P', 'V', 'W', 'Z', 'G', 'H', 'Q'],
         ['R', 'N', 'C', 'L', 'D', 'Z', 'G']
    ]
buildStack2T :: Stack2
buildStack2T =
    [
         ['N', 'Z'],
         ['D', 'C', 'M'],
         ['P']
    ]

move :: [Stack] -> Int -> Int -> [Stack]
move ls from to =
    let s1 = ls !! from 
        s2 = ls !! to
        t  = pop s1
    in toList $ mapWithIndex (\i v -> if i == from
                             then snd t
                             else if i == to
                                then push s2 (fst t)
                                else v) (fromList  ls)

move2 :: Stack2  -> Int -> Int -> Int -> Stack2
move2 ls qt from to =
    let s1 = ls !! from 
        s2 = ls !! to
        aux = Prelude.take qt s1 
    in toList $ mapWithIndex (\i v -> if i == from
                             then Prelude.drop qt s1 
                             else if i == to
                                then aux ++ s2
                                else v) (fromList  ls)

processMoves :: [Stack] -> Int -> Int -> Int -> [Stack]
processMoves s m f t =
    foldl (\acc x->move acc f t) s [1..m]

readPos :: [String] -> Int -> Int
readPos s p =
        read (s !! p) :: Int

main = do
    contents <- getContents
    --let r = foldl (\acc x-> processMoves acc (readPos x 1) (pred $ readPos x 3) (pred $ readPos x 5)) buildStack $ map (words) (lines contents) 
    let r2 = foldl (\acc x-> move2 acc (readPos x 1) (pred $ readPos x 3) (pred $ readPos x 5)) buildStack2 $ map (words) (lines contents)
        --a = foldl (\acc x-> (fst (pop x)):acc) [] r
        a = foldr (\x acc -> (head x):acc) [] r2
    --print . Prelude.reverse $ a
    print a

