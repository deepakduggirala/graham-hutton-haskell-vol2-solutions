data Prop =
    Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Imply Prop Prop
  | Or Prop Prop
  | Eqv Prop Prop

type Assoc k v = [(k, v)]

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = not (eval s p) || eval s q
eval s (Or p q) = eval s p || eval s q
eval s (Eqv p q) = eval s p == eval s q   --Q8

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q
vars (Eqv p q) = vars p ++ vars q   --Q8


bools :: Int -> [[Bool]]
bools 1 = [[True],[False]]
bools n = map (True:) (bools (n-1)) ++ map (False:) (bools (n-1))

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

find :: Char -> Subst -> Bool
find _ [] = False
find c ((k,v):kvs)
  | c == k = v
  | otherwise = find c kvs

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools.length $ vs)
  where
    vs = rmdups.vars $ p

isTaut :: Prop -> Bool
isTaut p = and [ eval s p | s <- substs p]