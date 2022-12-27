-- Build Data Type following the commands

data FileSystem = Dir String [FileSystem] | File Int String deriving (Show)

fileSize :: FileSystem -> Int
fileSize (File s _) = s
fileSize (Dir n f) =
    (sum (map (\x-> case x of File s _ -> s; Dir _ _ -> 0) f)) + (sum (map (\x-> case x of File _ _ -> 0; Dir n1 f1 -> fileSize (Dir n1 f1)) f)) 

groupFiles :: FileSystem -> [FileSystem]
groupFiles (File _ _) = []
groupFiles (Dir n f)  = (Dir n f):(foldl (\acc x-> (groupFiles x) ++  acc) [] f)

process :: FileSystem -> Int
process (File _ _) = 0
process f =
    let filesGrouped = groupFiles f
    in sum (filter (<= 100000) (map (fileSize) filesGrouped))
