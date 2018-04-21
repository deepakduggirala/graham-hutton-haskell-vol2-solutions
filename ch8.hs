--Q1
data Nat = Zero | Succ Nat deriving Show

add :: Nat -> Nat -> Nat
add Zero n = n
add n Zero = n
add (Succ n1) n2 = Succ (add n2 n1)

mul :: Nat -> Nat -> Nat
mul Zero _ = Zero
mul _ Zero = Zero
mul (Succ n1) n2 = add (mul n2 n1) n2

--Q3
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

balanced :: Tree a -> Bool
balanced (Leaf _ ) = True
balanced (Node l r) = leafCount l == leafCount r
  where
    leafCount :: Tree a -> Int
    leafCount (Leaf _) = 1
    leafCount (Node l r) = leafCount l + leafCount r

-- Q4
-- halve from chapter 4
halve :: [a] -> ([a], [a])
halve xs =
  (take n xs, drop n xs)
    where
      n = length xs `div` 2

balance :: [a] -> Tree a
balance [x] = Leaf x
balance xs = Node (balance left) (balance right)
  where
    (left, right) = halve xs

--Q5
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val Int) = f Int
folde f g (Add expr1 expr2) = g expr1 expr2

--Q7
instance Eq a => Eq (Maybe a) where
  Nothing == Nothig = True
  (Just a) == (Just b) = a == b

instance Eq a => Eq [a] where
  [] == [] = True
  (x:xs) == (y:ys) = x == y && xs == ys