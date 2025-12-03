-- this uses a lot of RAM btw, so I couldn't
-- actually test it as it gets OOM killed lmao

import Data.List.Split

isInvalid :: Integer -> (Integer,Bool)
isInvalid x = (x, length p == 2 && p1 == p2)
  where
        s = show x
        p = chunksOf ((length s)`div`2) s
        p1 = p!!0
        p2 = p!!1

checkRange :: String -> [(Integer,Bool)]
checkRange s = filter (\(x,y) -> y /= False) $ map isInvalid [start..end]
  where
        range = splitOn "-" s
        start = read (range!!0)::Integer
        end = read (range!!1)::Integer

main :: IO ()
main = do
    file <- readFile "input.txt"
    print $ sum $ map fst $ concat $ map checkRange $ splitOn "," file
