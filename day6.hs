import Data.List

startOfPacket :: String -> Int -> Int
startOfPacket s i =
    if (length (nub $ take 14 s)) == 14
        then
            i + 14
        else
            startOfPacket (drop 1 s) i + 1
