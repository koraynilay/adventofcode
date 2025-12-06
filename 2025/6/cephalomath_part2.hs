import Data.List.Split

doOp :: [[Integer]] -> [(Integer -> Integer -> Integer, Integer)] -> [Integer]
doOp [] [] = []
doOp (x:xs) (o:op) = foldl (fst o) (snd o) x : doOp xs op

convOps :: String -> [(Integer -> Integer -> Integer, Integer)]
convOps xs = map toOp $ words xs
  where
        toOp :: String -> ((Integer -> Integer -> Integer), Integer)
        toOp s | s == "+" = ((+), 0)
               | s == "*" = ((*), 1)

convNum :: [String] -> [[Integer]]
convNum [] = []
convNum (x:xs) = map (read::String->Integer) (words x) : convNum xs

{-

these 2 functions take a list of lists, where each sublist
should be concatenated to the one after, but only up to an empty list,
which serves as delimiter, like so:
     [  [0],[1],[2], [], [3],[4], [], [5], ... ] -> splitMerge (splitOn)
  -> [ [[0],[1],[2]],   [[3],[4]],   [[5]],... ] -> mergeInner
  -> [  [0,1,2],         [3,4],       [5], ... ]

-}

splitMerge :: [[Integer]] -> [[Integer]]
splitMerge xs = mergeInner $ splitOn [[]] xs

mergeInner :: [[[Integer]]] -> [[Integer]]
mergeInner [] = []
mergeInner (y:ys) = concat y : mergeInner ys

-- https://stackoverflow.com/q/2578930
transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

main :: IO ()
main = do
        file <- readFile "input.txt"
        let ll = lines file
        -- unlike in part 1, where it needs to get transposed after
        -- being converted to Integer(s), this needs to be transposed
        -- first in order to read the numbers correctly without losing
        -- informations about position provided by the extra spaces
        --
        -- doing this, also makes the columns of spaces into row(s)::String
        -- which then get converted into []
        let nn = splitMerge $ convNum $ transpose $ init ll
        let pp = convOps $ last ll
        print $ sum $ doOp nn pp
