drop' :: Int -> [a] -> [a]
drop' n [] = []
drop' 0 xs = xs
drop' n (x:xs) = drop (n-1) xs

--Q1
fac :: Int -> Int
fac 0 = 1
fac n
  | n > 0 = n * fac (n-1)

--Q2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

--Q3
(^) :: Int -> Int -> Int
_ ^ 0 = 1
m ^ 1 = m
m ^ n = m * ( m Main.^ (n-1))

--Q4
euclid :: Int -> Int -> Int
euclid _ 1 = 1
euclid m n
  | m < n = euclid n m
  | m == n = m
  | otherwise = euclid (m-n) n

--Q6
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x:(replicate (n-1) x)

(!!) :: [a] -> Int -> a
(x:xs) !! 0 = x
(x:xs) !! i = xs Main.!! (i-1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' y (x:xs)
  | y == x = True
  | otherwise = elem' y xs

--Q7
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x < y = x:( merge xs (y:ys))
  | x == y = x:(y:(merge xs ys))
  | otherwise = y:(merge (x:xs) ys)

--Q8
msort :: Ord a => [a] -> [a]
msort [] = []
msort (x:[]) = [x]
msort xs = merge (msort left) (msort right)
  where
    (left, right) = splitAt n xs
      where
        n = (length xs) `div` 2

--Q9
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

take' :: Int -> [a] -> [a]
take' _ [] = []
take' 0 xs = []
take' n (x:xs) = x:(take' (n-1) xs)

last' :: [a] -> a
last' (x:[]) = x
last' (x:xs) = last' xs