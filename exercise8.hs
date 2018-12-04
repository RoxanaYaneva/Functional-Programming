fact :: Integral a => a -> a
fact 0 = 1
fact n = n *  fact (n - 1)


fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

fibonacci' :: Integral a => a -> a
fibonacci' n = loop 0 0 1
    where loop i curr next
           | i == n    = curr
           | otherwise = loop (i + 1) next (curr + next)


countRoots :: (Num a, Ord a, Eq a) => a -> a -> a -> String
countRoots a b c
    | d < 0       = "No roots"
    | d > 0       = "Two roots"
    | otherwise   = "One root"
    where d = b^2 - 4 * a * c


power :: (Num a, Integral n) => a -> n -> a
power a n
    | n == 1          = a
    | mod n 2 == 0    = power (a * a) b
    | otherwise       = a * power (a * a) b
    where b = div n 2


gcd' :: Integral a => a -> a -> a
gcd' 0 b = b
gcd' a 0 = a
gcd' a b 
    | a == b     = a
    | a > b      = gcd' (a - b) b
    | otherwise  = gcd' a (b - a)


lcm' :: Integral a => a -> a -> a
lcm' a b = div (a * b) (gcd' a b)

 
ackerman m n
    | m == 0             = n + 1
    | m > 0 && n == 0    = ackerman (m - 1) 1
    | otherwise          = ackerman (m - 1) (ackerman m (m - 1))


distance :: Floating a => (a, a) -> (a, a) -> a
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)


modulus :: Floating a => (a, a) -> a
modulus = distance (0, 0)


complAdd :: Floating a => (a, a) -> (a, a) -> (a, a)
complAdd (a, b) (c, d) = (a + c, b + d)


complSub :: Floating a => (a, a) -> (a, a) -> (a, a)
complSub (a, b) (c, d) = (a - c, b - d)


complMul :: Floating a => (a, a) -> (a, a) -> (a, a)
complMul (a, b) (c, d) = (a * c - b * d, a * d + b * c)
