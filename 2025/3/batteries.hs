import Data.List

fixEsc :: [(Integer,Int)] -> Integer
fixEsc xs = (fst a1)*10 + (fst a2)
  where
        a1 = maxf fst $ reverse $ init xs
        a2 = maxf fst $ drop ((snd a1)+1) xs

maxf :: Ord b => (a -> b) -> [a] -> (a)
maxf f = head . reverse . sortOn f

addIndex :: [Integer] -> [(Integer,Int)]
addIndex = addIndexAux 0

addIndexAux :: Int -> [Integer] -> [(Integer,Int)]
addIndexAux _ [] = []
addIndexAux i (x:xs) = (x,i) : (addIndexAux (i+1) xs)

f line = fixEsc $ addIndex $ map (read.pure::Char->Integer) line

main :: IO ()
main = do
    file <- readFile "input.txt"
    print $ sum $ map f $ lines file
