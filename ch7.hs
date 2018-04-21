--Q1
test1 f p = map f . filter p

--Q2
all' :: (a -> Bool) -> [a] -> Bool
all' p = foldr f True
  where
    f x y = p x && y

any' :: (a -> Bool) -> [a] -> Bool
any' p = foldr f False
  where
    f x y = p x || y

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs)
  | p x = x:(takeWhile' p xs)
  | otherwise = []

takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' p = foldr f []
  where
    f h t
      | p h = h:t
      | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs)
  | p x = dropWhile' p xs
  | otherwise = x:xs


--Q3
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\ h t -> (f h) : t ) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\ h t -> if p h then h:t else t) []

--Q4
dec2int :: [Int] -> Int
dec2int = foldr (\ h t -> h + 10*t) 0 . reverse

--Q5
curry' :: ((a,b) -> c) -> a -> b -> c
curry' f = \ x -> \ y -> f (x, y)

uncurry' :: (a -> b -> c) -> (a,b) -> c
uncurry' f = \ (x,y) -> f x y

--Q6
unfold p h t x
  | p x  = []
  | otherwise  = h x : unfold p h t (t x)

map2 :: (a -> b) -> [a] -> [b]
map2 f = unfold p h t
  where
    p = null
    h = f.head
    t = tail

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate f (f x)

iterate2 :: (a -> a) -> a -> [a]
iterate2 f = unfold p id t
  where
    p = \x -> False
    t = f

--Q9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g [] = []
altMap f g [x] = [f x]
altMap f g (x:y:t) = f x : g y : (altMap f g t)

--Q10

luhnDouble :: Int -> Int
luhnDouble x =
  if x2 > 9 then
    x2 - 9
  else x2
    where
      x2 = x * 2

luhn :: [Int] -> Bool
luhn = divisibleBy10 . sum . altMap id luhnDouble . reverse
  where
    divisibleBy10 x = x `mod` 10 == 0