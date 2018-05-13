newtype ST s a = S (s -> (a, s) )

app :: ST s a -> s -> (a, s)
app (S st) s = st s

instance Functor (ST s) where
  -- fmap :: (a->b) -> ST a -> ST b
  fmap f st = S( \s -> let (x, s') = app st s in (f x, s') )

instance Applicative (ST s) where
  -- pure :: a -> ST a
  pure x = S (\s -> (x, s))

  -- (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> st = S(\s ->
    let
      (f, s') = app stf s
      (x, s'') = app st s'
    in
      (f x, s''))

instance Monad (ST s) where
  -- return :: a -> ST a
  return = pure

  -- (>>)= :: ST a -> (a -> ST b) -> ST b
  st >>= f = S( \s ->
    let
      (x, s') = app st s
    in
      app (f x) s')