import Data.List
import Data.Ord (comparing)

quickSortCompare :: Ord a => (a -> a -> Ordering) -> [a] -> [a]
quickSortCompare _ []       = []
quickSortCompare compare (x:xs)   = quickSortCompare compare less ++ [x] ++ quickSortCompare compare greater
    where less     = filter (<= x) xs
          greater  = filter (> x) xs 

quickSort :: Ord a => [a] -> [a]
quickSort []       = []
quickSort (x:xs)   = quickSort less ++ [x] ++ quickSort greater
    where less     = filter (<= x) xs
          greater  = filter (> x) xs 

twice :: (Int -> Int) -> Int -> Int
twice f x = f $ f x

sumOfSquares = [sum | x <- [1..], y <- [1..], let sum = x^2 + y^2]

average xs = (sum xs) / (fromIntegral (length xs))

comparePair :: (String, Int) -> (String, Int) -> Ordering
comparePair (_, x) (_, y)
    | x < y          = LT
    | x > y          = GT
    | otherwise      = EQ

data Pair = Pair String Int

instance Eq Pair where
    (Pair x1 y1) == (Pair x2 y2) = y1 `mod` 5 == y2 `mod` 5
instance Ord Pair where
    (Pair x1 y1) <= (Pair x2 y2) = y1 `mod` 5 <= y2 `mod` 5
instance Show Pair where
    show (Pair x y) = "(" ++ show x ++ "," ++ show y ++ ")"

specialSort :: [Pair] -> [Pair]
specialSort = quickSort

type Pair2 = (String, Int)
specialSort2 :: [Pair2] -> [Pair2]
specialSort2 xs = sortBy (comparing f) xs
    where f = ((`mod` 6).snd)

