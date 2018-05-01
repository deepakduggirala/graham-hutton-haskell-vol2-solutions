-- Q1

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap f Leaf = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)


-- Q2
data T z a = T (z -> a)
instance Functor ( T z ) where
  -- fmap :: (a -> b) -> (z->a) -> (z->b)
  fmap f (T g) = T(f.g)

-- Q3
instance Applicative (T z) where
  -- pure :: a -> T z a
  pure a = T(\z -> a)

  -- (<*>) :: (T z (a -> b)) -> (T z a) -> (T z b)
  -- f :: z -> a -> b
  -- g :: z -> a
  (<*>) (T f) (T g) = T (\z -> f z (g z))

  --Q5
  -- pure id <*> x = x
    --  id :: a -> a
    --  pure :: a -> M a
    --  (<*>) :: M (a -> b) -> M a -> M b
    --  x :: M a

  -- pure (g x) = pure g <*> pure x
    --  x :: a
    --  g :: a -> b
  
  -- x <*> pure y = pure (\g -> g y) <*> x
    --  x :: M (a -> b)
    --  y :: a
    --  g :: a -> b

  -- x <*> (y <*> z) = (x <*> y) <*> z
    --  x :: M (a -> b -> c)
    --  y :: M a
    --  z :: M b

-- Q6
instance Monad (T z) where
  -- return :: a -> T z a
  return a = T (\z -> a)

  -- (>>=) :: (T z a) -> (a -> (T z b)) -> (T z b)
  T m >>= f = joinT (T (\z -> f (m z)))

joinT :: T z (T z a) -> T z a
joinT (T f) = T ( \z -> case (f z) of
    T g -> (g z)
  )

-- Q7

data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

instance Functor Expr where
  -- fmap :: (a -> b) -> (Expr a) -> (Expr b)
  fmap f (Var x) = Var (f x)
  fmap f (Val n) = Val n
  fmap f (Add l r) = Add (fmap f l) (fmap f r)

instance Applicative Expr where
  -- pure :: a -> Expr a
  pure x = Var x

  -- (<*>) :: Expr (a -> b) -> (Expr a) -> (Expr b)
  (Var f) <*> (Var x) = Var (f x)
  _ <*> (Val n) = Val n
  (Val n) <*> e = Val n
  (Add ef eg) <*> e = Add (ef <*> e) (eg <*> e)
  ef <*> (Add l r) = Add (ef <*> l) (ef <*> r)

instance Monad Expr where
  -- return :: a -> Expr a
  return = pure

  -- (>>=) :: (Expr a) -> (a -> Expr b) -> Expr b
  (Var x) >>= f = f x
  (Val n) >>= f = Val n
  (Add l r) >>= f = Add (l >>= f) (r >>= f)


-- Q8
type State = Int
newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
  -- fmap :: (a->b) -> ST a -> ST b
  fmap g st = do
    x <- st
    return (g x)

instance Applicative ST where
  -- pure :: a -> ST a
  pure = return

  -- (<*>) :: ST (a->b) -> ST a -> ST b
  stf <*> stx = do
    a <- stx
    f <- stf
    return (f a)

instance Monad ST where
  -- return :: a -> ST a
  return x = S (\state -> (x, state))

  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= g = S (\state ->
    let
      (x, state') = app st state
    in
      app (g x) state')