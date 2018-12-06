replicate' :: Integral a => a -> b -> [b]
replicate' 0 _       = []
replicate' n (x:xs)  = x : replicate' (n - 1) xs


minimum' :: Ord a => [a] -> a
minimum' xs = foldr min (head xs) xs


maximum' :: Ord a => [a] -> a
maximum' xs = foldr max (head xs) xs


reverse' :: [a] -> [a]
reverse' xs = foldl (\x y -> y : x) [] xs


length' :: [a] -> Int
length' xs = foldr (\_ n -> n + 1) 0 xs


all' :: Eq a => (a -> Bool) -> [a] -> Bool
all' p xs = xs == (foldr (\x xs -> if p x then x : xs else xs) [] xs)
--all' p xs = xs == filter p xs


any' :: (a -> Bool) -> [a] -> Bool
any' p xs = (not (null (foldr (\x res -> if p x then x:res else res) [] xs)))
--any' p xs = not $ null $ filter p xs


replicate'' :: Integral a => a -> b -> [b]
replicate'' x y = foldr (\_ ys -> y : ys) [] [1..x]

make_set :: Eq a => [a] -> [a]
make_set xs = foldr (\x xs -> if elem x xs then xs else x:xs) [] xs


sum_divisors :: Integral a => a -> a
sum_divisors n = sum [i | i <- [1..n], mod n i == 0]


is_prime :: Integral a => a -> Bool
is_prime n = length [i | i <- [2..sqrt' n], mod n i == 0] == 1
    where sqrt' = floor $ sqrt $ fromIntegral


decartes :: [a] -> [a] -> [(a, a)]
decartes xs ys = [(x, y) | x <- xs, y <- ys]


primes = [x | x <- [2..], is_prime x]


natural_pairs = [(x,y) | x <- [1..], y <- [1..]]


pythagorean_triples = [(a, b, c) | c <- [5..], b <- [4..c], a <- [3..c], a < b, a^2 + b^2 == c^2]


compress :: Eq a => [a] -> [(a, Int)]
compress xs = loop 1 xs
         where loop cnt xs 
                | null xs                     = []
                | null (tail xs)              = [(head xs, cnt)]
                | head xs == head (tail xs)   = loop (cnt + 1) (tail xs)
                | otherwise                   = [(head xs, cnt)] ++ compress (tail xs) 


max_repeated :: Eq a => [a] -> Int
max_repeated xs = maximum (map snd (compress xs))


histogram :: Eq a => [a] -> [(a, Int)]
histogram xs = foldr (\y ys -> ys ++ [(y, count y xs)]) [] (make_set xs)
    where count x xs = length $ filter (==x) xs


max_distance :: [(Double, Double)] -> Double
max_distance xs = maximum [distance | (x1, y1) <- xs, (x2, y2) <- xs, 
                                      (x1, y1) /= (x2, y2)
                                       let distance = sqrt ((x2 - x1)^2 + (y2 - y1)^2)]
