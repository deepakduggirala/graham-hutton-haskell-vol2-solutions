import Data.Monoid()
import Data.Foldable
-- Q1
-- instance (Monoid a, Monoid b) => Monoid (a,b) where
--   -- mempty :: (a,b)
--   mempty = (mempty a, mempty b)
--   -- mappend :: (a,b) -> (a,b) -> (a,b)
--   (a1,b1) `mappend` (a2,b2) = ( a1 `mappend` a2, b1 `mappend` b2 )

-- Q2
-- instance Monoid b => Monoid (a->b) where
--   -- mempty :: (a->b)
--   mempty = \_-> mempty
--   -- mappend :: (a->b) -> (a->b) -> (a->b)
--   f `mappend` g = \a -> (f a) `mappend` (g a)

-- Q3
-- instance Foldable Maybe where
--   -- fold :: Monoid a => t a -> a
--   fold Nothing = mempty
--   fold (Just x) = x
--   -- foldMap :: Monoid b => (a->b) -> t a -> b
--   foldMap f Nothing = mempty
--   foldMap f (Just x) = f x
--   -- foldr :: (a -> b -> b) -> b -> t a -> a
--   foldr _ start Nothing = start
--   foldr f _ (Just x) = f x
--   -- foldl :: (b -> a -> b) -> b -> t a -> b
--   foldl _ start Nothing = start
--   foldl f _ (Just x) = f x

data Maybe' a = Nothing' | Just' a
instance Foldable Maybe' where
  -- fold :: Monoid a => t a -> a
  fold Nothing' = mempty
  fold (Just' x) = x
  -- foldMap :: Monoid b => (a->b) -> t a -> b
  foldMap _ Nothing' = mempty
  foldMap f (Just' x) = f x
  -- foldr :: (a -> b -> b) -> b -> t a -> b
  foldr _ start Nothing' = start
  foldr f start (Just' x) = f x start
  -- foldl :: (b -> a -> b) -> b -> t a -> b
  foldl _ start Nothing' = start
  foldl f start (Just' x) = f start x

-- Q4
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

instance Foldable Tree where
  -- fold :: Monoid a => t a -> a
  fold Leaf = mempty
  fold (Node left x right) = (fold left) `mappend` x `mappend` (fold right)
  
  -- foldMap :: Monoid b => (a->b) -> t a -> b
  foldMap _ Leaf = mempty
  foldMap f (Node left x right) = (foldMap f left) `mappend` (f x) `mappend` (foldMap f right)

  -- foldr :: (a -> b -> b) -> b -> t a -> b
  foldr _ start Leaf = start
  foldr f start (Node left x right) = foldr f (f x (foldr f start right)) left

  -- -- foldl :: (b -> a -> b) -> b -> t a -> b
  foldl _ start Leaf = start
  foldl f start (Node left x right) = foldl f ( f (foldl f start left) x) right

-- instance Traversable Tree where
--   -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
--   traverse g Leaf = pure Leaf
--   traverse g (Node left x right) = g <*> 

-- Q5
filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF pred tx = map fst $ filter snd $ foldMap (\x -> [(x, pred x)]) tx