import Data.List
import Data.List.Split

checkIngredients :: [Integer] -> Integer -> Integer -> [Integer]
checkIngredients [] _ _ = []
checkIngredients (x:xs) s e | isFresh x = x : checkIngredients xs s e
                            | otherwise = checkIngredients xs s e
  where isFresh n = n >= s && n <= e

getRanges :: [String] -> [(Integer,Integer)]
getRanges ("":xs) = []
getRanges (r:rs) = getRange r ++ getRanges rs

getRange :: String -> [(Integer,Integer)]
getRange s = [(start,end)]
  where
        range = splitOn "-" s
        start = read (range!!0)::Integer
        end = read (range!!1)::Integer

-- basically, if x and y aren't overlapping it will add x to the result
-- if they're overlapping it make and keep the "temporary" range until it
-- becomes non overlapping compared to the one after, which means that
-- specific range has finished
-- this is correct because the input is sorted, which means that
-- fst x <= fst x+1, this also means we don't need extra logic for
-- min fst like we do for max snd
checkRanges :: [(Integer,Integer)] -> [(Integer,Integer)]
checkRanges = checkRangesAux . sort
  where
        checkRangesAux :: [(Integer,Integer)] -> [(Integer,Integer)]
        checkRangesAux (x:[]) = [x]
        checkRangesAux (x:y:xs) | noOverlap = newRange : checkRangesAux (c:xs)
                                | otherwise = checkRangesAux (c:xs)
          where
                noOverlap = (snd x) < (fst y)
                newRange | noOverlap = x
                         | otherwise = (newStart, newEnd)
                newStart = fst x
                newEnd   = max (snd x) (snd y)
                c | noOverlap = y
                  | otherwise = newRange

-- this just gets number of items in each range
-- +1 because they're inclusive
sumRanges :: [(Integer,Integer)] -> Integer
sumRanges [] = 0
sumRanges (x:xs) = (snd x) - (fst x) + 1 + sumRanges xs

main :: IO ()
main = do
        file <- readFile "input.txt"
        print $ sumRanges $ checkRanges $ sort $ getRanges $ lines file
