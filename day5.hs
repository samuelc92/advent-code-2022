infixr 5 :-:
data Stack = Empty | Char :-: (Stack) deriving (Show, Read, Eq, Ord)

push :: Stack -> Char -> Stack
push Empty a  = a :-: Empty 
push s a = a :-: s 

pop :: Stack -> (Char, Stack)
pop Empty = (' ', Empty)
pop (x:-:xs) = (x, xs)


buildStack :: [Stack]
buildStack =
    [
         'Q' :-: 'F' :-: 'L' :-: 'S' :-: 'R' :-: Empty,
         'T' :-: 'P' :-: 'G' :-: 'Q' :-: 'Z' :-: 'N' :-: Empty,
         'B' :-: 'Q' :-: 'M' :-: 'S' :-: Empty,
         'Q' :-: 'B' :-: 'C' :-: 'H' :-: 'J' :-: 'Z' :-: 'G' :-: 'T' :-: Empty,
         'S' :-: 'F' :-: 'N' :-: 'B' :-: 'M' :-: 'H' :-: 'P' :-: Empty,
         'G' :-: 'V' :-: 'L' :-: 'S' :-: 'N' :-: 'Q' :-: 'C' :-: 'P' :-: Empty,
         'F' :-: 'C' :-: 'W' :-: Empty,
         'M' :-: 'P' :-: 'V' :-: 'W' :-: 'Z' :-: 'G' :-: 'H' :-: 'Q' :-: Empty,
         'R' :-: 'N' :-: 'C' :-: 'L' :-: 'D' :-: 'Z' :-: 'G' :-: Empty
    ]

move :: [Stack] -> Int -> Int -> Stack
move ls from to =
    let s1 = ls !! from 
        s2 = ls !! to
        t  = pop s1
    in push s2 (fst t)

--process :: [Stack] -> Int -> Int -> Int -> [Stack]
--process s m f t =

