import Data.List

fixEsc :: [(Integer,Int)] -> Integer
fixEsc xs = read $ concat $ map show $ (fst a1) : a2 :: Integer
  where
        a1 = maxf fst $ take ((length xs)-11) xs
        a2 = aaaa 10 a1 xs

aaaa :: Int -> (Integer,Int) -> [(Integer,Int)] -> [Integer]
aaaa n mm xs | n == 0 = [fst ab]
             | otherwise = (fst ab) : aaaa (n-1) ab xs
  where
        ab = maxf fst $ drop ((snd mm)+1) $ take ((length xs)-n) xs

maxf :: Ord b => (a -> b) -> [a] -> (a)
maxf f = head . reverse . sortOn f . reverse
-- rightmost reverse is to have the indexes increasing

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
