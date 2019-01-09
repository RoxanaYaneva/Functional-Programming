import Data.List
import Data.Function

sumProducts :: Num a => [[a]] -> a
sumProducts ll = sum [prod | l <- ll, let prod = foldr (*) 1 l]

occurrences :: (Num a, Eq a) => [a] -> [a] -> [Int]
occurrences xs ys = [count | x <- xs, let count = length $ filter (==x) ys]

matchLengths :: (Num a, Eq a) => [[a]] -> Bool
matchLengths ll = and $ map (== head l) (tail l)
    where l = map length ll

makeSet :: Eq a => [a] -> [a]
makeSet xs = foldr (\x xs -> if elem x xs then xs else x:xs) [] xs

setUnion :: Ord a => [a] -> [a] -> [a]
setUnion xs [] = xs
setUnion [] ys = ys
setUnion xs ys = makeSet $ xs ++ ys

setIntersect :: Ord a => [a] -> [a] -> [a]
setIntersect xs [] = []
setIntersect [] ys = []
setIntersect xs ys = [x | x <- xs, x `elem` ys]

setDiff :: Ord a => [a] -> [a] -> [a]
setDiff xs [] = xs
setDiff [] ys = []
setDiff xs ys = [x | x <- xs, not $ x `elem` ys]

--crossOut :: Num a => [[a]] -> [[a]]

--specialSort :: Ord a => [[a]] -> [[a]]

mostCommon :: Ord a => [a] -> a
mostCommon = head . maximumBy (compare `on` length) . group . sort
