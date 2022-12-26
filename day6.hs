import Data.List

startOfPacket :: String -> Int -> Int
startOfPacket s i =
    if (length (nub $ take 4 s)) == 4
        then
            i + 4
        else
            startOfPacket (drop 1 s) i + 1
