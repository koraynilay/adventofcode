import Data.List
import Data.List.Split

-- creates list of fresh ingredients for the [s..e] range
checkIngredients :: [Integer] -> Integer -> Integer -> [Integer]
checkIngredients [] _ _ = []
checkIngredients (x:xs) s e | isFresh x = x : checkIngredients xs s e
                            | otherwise = checkIngredients xs s e
  where isFresh n = n >= s && n <= e

getIngredients :: [String] -> [Integer]
getIngredients ("":xs) = convIngridients xs
  where
        convIngridients :: [String] -> [Integer]
        convIngridients [] = []
        convIngridients (x:xs) = (read x::Integer) : convIngridients xs
getIngredients (x:xs)  = getIngredients xs

-- `is` is the ingredients list, this function will make a list of all the fresh
-- ingredients for all the ranges, with duplicates (which will need to be removed after)
checkRanges :: [Integer] -> [String] -> [Integer]
checkRanges _ ("":ls) = []
checkRanges is (l:ls) = checkIngredients is start end ++ checkRanges is ls
  where
        range = splitOn "-" l
        start = read (range!!0)::Integer
        end = read (range!!1)::Integer

-- Source - https://stackoverflow.com/a/16109302
-- Posted by scvalex, modified by community. See post 'Timeline' for change history
-- Retrieved 2025-12-06, License - CC BY-SA 3.0
rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

main :: IO ()
main = do
        file <- readFile "input.txt"
        let ll = lines file
        let is = getIngredients ll
        print $ length $ rmdups $ checkRanges is ll
