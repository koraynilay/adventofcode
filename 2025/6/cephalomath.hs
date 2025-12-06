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

-- https://stackoverflow.com/q/2578930
transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

main :: IO ()
main = do
        file <- readFile "input.txt"
        let ll = lines file
        let nn = transpose $ convNum $ init ll
        let pp = convOps $ last ll
        print $ sum $ doOp nn pp
