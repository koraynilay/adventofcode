import Data.List

fixEsc :: [(Int,Integer)] -> Integer
fixEsc xs = (snd a1)*10 + (snd a2)
  where
        ss = reverse $ sortOn snd $ reverse xs
        mx = ss!!0
        a1 | fst mx == ((length ss)-1) = ss!!1
           | otherwise = mx
        a2 | fst mx == ((length ss)-1) = mx
           | otherwise = (bb ss)!!0
        bb (c:cc) = reverse $ sortOn snd $ filter (\(x,y) -> x > (fst a1)) cc

addIndex :: [Integer] -> [(Int, Integer)]
addIndex = addIndexAux 0

addIndexAux :: Int -> [Integer] -> [(Int, Integer)]
addIndexAux _ [] = []
addIndexAux i (x:xs) = (i,x) : (addIndexAux (i+1) xs)

f line = fixEsc $ addIndex $ map (read.pure::Char->Integer) line

main :: IO ()
main = do
    file <- readFile "input.txt"
    print $ sum $ map f $ lines file

{- logic of the thing:

if ss!!0 is last
   a1 = ss!!1
   a2 = ss!!0

if ss!!0 is not last
   a1 = ss!!0
   a2 = filter (> (fst a1)) (cc)

-}
