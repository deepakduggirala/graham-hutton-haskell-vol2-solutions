halve :: [a] -> ([a], [a])
halve xs =
  (take n xs, reverse $ take n $ reverse xs)
    where
      n = length xs `div` 2

-- Q2
third1 :: [a] -> a
third1 = head . tail . tail

third2 :: [a] -> a
third2 xs = xs !! 2

third3 :: [a] -> a
third3 (x1:(x2:(x3:xs))) = x3

--Q3
safeTail1 :: [a] -> [a]
safeTail1 xs =
  if null xs then
    []
  else
    tail xs

safeTail2 :: [a] -> [a]
safeTail2 xs
  | null xs = []
  | otherwise = tail xs
  
  
safeTail3 :: [a] -> [a]
safeTail3 [] = []
safeTail3 (x:xs) = xs

--Q5
and :: Bool -> Bool -> Bool
and b1 b2 =
  if b1 then
    if b2 then
      True
    else False
  else False


--Q8
luhnDouble :: Int -> Int
luhnDouble x =
  if x2 > 9 then
    x2 - 9
  else x2
    where
      x2 = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d =
  let total = luhnDouble a + b + luhnDouble c + d in
    (total `mod` 10) == 0