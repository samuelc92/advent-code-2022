import Data.List 

type Name = String
type Bytes = Int
data FSystem= Dir Name [FSystem] | File Name Bytes deriving (Show) 

data Breadcrumb = Breadcrumb Name [FSystem] deriving (Show)
type Zipper = (FSystem, [Breadcrumb])


up :: Zipper -> Zipper
up (item, Breadcrumb name ls:xs) =
    (Dir name (ls ++ [item]), xs)

myDisk :: FSystem
myDisk =
    Dir "/"
        [ File "b.txt" 14848514
        , File "h.lst" 62596,
          Dir "a"
            [ File "f" 29116
            , Dir "e"
                [ File "i" 584]
            ]
        ]

to :: FSystem -> Zipper -> Zipper
to f (Dir name l, bs) =
    (f, Breadcrumb name l:bs)

cmd :: String -> Zipper -> Zipper
cmd c (Dir dirName items, bs) =
    case (words c) of
        ("$":"cd":"..":_) -> up (Dir dirName items, bs)
        ("$":"cd":ys) -> to (Dir (last ys) []) (Dir dirName items, bs)
        ("$":"ls":_) -> (Dir dirName items, bs)
        ("dir":_) -> (Dir dirName items, bs)
        (x:xs) -> (Dir dirName ((File (last xs) (read x :: Int)):items), bs)

fileSize :: FSystem -> Int
fileSize (File _ s) = s
fileSize (Dir n f) =
    (sum (map (\x-> case x of File _ s -> s; Dir _ _ -> 0) f)) + (sum (map (\x-> case x of File _ _ -> 0; Dir n1 f1 -> fileSize (Dir n1 f1)) f))

groupFiles :: FSystem -> [FSystem]
groupFiles (File _ _) = []
groupFiles (Dir n f)  = (Dir n f):(foldl (\acc x-> (groupFiles x) ++  acc) [] f)

process :: FSystem -> Int
process (File _ _) = 0
process f =
    let filesGrouped = groupFiles f
    in sum (filter (<= 100000) (map (fileSize) filesGrouped))

process2 :: FSystem -> Int -> Int
process2 (File _ _) _ = 0
process2 f s =
    let filesGrouped = groupFiles f
    in minimum (filter (>= (30000000 - (70000000 - s))) (map (fileSize) filesGrouped))


main = do
    contents <- getContents
    let rootFSystem  = Dir "/" []
        filesSystem  = foldl (\acc x-> cmd x acc) (rootFSystem, []) (drop 1 $ lines contents)
        root         = (fst (foldl (\acc _->up acc) filesSystem (snd filesSystem)))
        --result       = process (fst (foldl (\acc _->up acc) filesSystem (snd filesSystem)))
        totalSize    = fileSize root
    print $ process2 root totalSize 
