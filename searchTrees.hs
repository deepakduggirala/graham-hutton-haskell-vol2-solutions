data BTree a =
    Empty
  | Leaf a
  | Node (BTree a) a (BTree a)
  deriving Show

construct :: Ord a => [a] -> BTree a
construct [] = Empty
construct [x] = Leaf x
construct (x:xs) = Node (construct smaller) x (construct larger)
  where
    smaller = filter (<=x) xs
    larger = filter (>x) xs


flatten :: BTree a -> [a]
flatten Empty = []
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

occurs:: Ord a => a -> BTree a -> Bool
occurs _ Empty = False
occurs x (Leaf y) = x == y
occurs x (Node l y r)
 | x == y = True
 | x < y = occurs x l
 | x > y = occurs x r