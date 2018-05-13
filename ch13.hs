import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) s = p s

item :: Parser Char
item = P (\inp -> case inp of
  [] -> []
  (x:xs) -> [(x, xs)])

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P (\inp -> case (parse p inp) of
    [] -> []
    [(v, out)] -> [(g v, out)])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure x = P (\inp -> [(x,  inp)])
  
  -- (<*>) :: Parser (a->b) -> Parser a -> Parser b
  pg <*> px = P (\inp -> case (parse pg inp) of
    [] -> []
    [(g, out)] -> parse (fmap g px) out)

instance Monad Parser where
  return = pure

  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= g = P (\inp -> case parse p inp of
    [] -> []
    [(v, out)] -> parse (g v) out)

three :: Parser (Char, Char)
three = do 
  x <- item
  item
  z <- item
  return (x, z)

-- three' :: Parser (Char, Char)
-- three' =  item >>= (\x ->
--             item >>= (\y ->
--               item >>= (\z ->
--                 return (x,z)
--               )
--             )
--           )

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (const [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\inp -> case parse p inp of
    [] -> parse q inp
    [(v, out)] -> [(v, out)])

sat :: (Char -> Bool) -> Parser Char
sat pred = do
  x <- item
  if pred x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (x==)

string :: String -> Parser String
string [] = return []
string (x:xs) = do
  char x
  string xs
  return (x:xs)

-- string (x:xs) =
--   char >>= (\x ->
--     string xs >>= (\_ ->
--       return (x:xs)
--     )
--   )

{-
many :: f a -> f [a]
some :: f a -> f [a]

in this context

many :: Parser a -> Parser [a]
some :: Parser a -> Parser [a]

alphanum :: Parser Char
many alphanum :: Parser String
-}

-- identifier as in code (token)
ident :: Parser String
ident = do
  x <- lower
  xs <- many alphanum
  return (x:xs)

nat :: Parser Int
nat = fmap read (some digit)

{-
  nat = do
    read <$> some digit
-}

{-
  xs <- some digit
  return (read xs)
-}

{-
read :: Read a => String -> a
read "123" :: Int = 123

the type of return should be Parser Int and return :: a -> Parser a, so input to return should be Int
that tells read that the should be Int
-}

space :: Parser ()
space = do
  many (sat isSpace)
  return ()

int :: Parser Int
int = do
  char '-'
  x <- nat
  return (-x)
  <|> nat

token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol s = token (string s)

nats :: Parser [Int]
nats = do
  symbol "["
  n <- natural
  ns <- many (do
    symbol ","
    natural)
  symbol "]"
  return (n:ns)

{-
expr ::= term + expr | term
term ::= factor * term | factor
factor ::= ( expr ) | nat
nat ::= 0 | 1 | 2 | ...

expr ::= term (+ expr | e)
term ::= factor (* term | e)
factor ::= ( expr ) | nat
nat ::= 0 | 1 | 2 | ...
-}

expr :: Parser Int
expr = do
  t <- term
  do
    symbol "+"
    e <- expr
    return (t+e)
    <|> return t

term :: Parser Int
term = do
  f <- factor
  do
    symbol "*"
    t <- term
    return (f*t)
    <|> return f

factor :: Parser Int
factor = do
  symbol "("
  e <- expr
  symbol ")"
  return e
  <|> natural

eval :: String -> Int
eval s = case parse expr s of
  [(x, [])] -> x
  [] -> error "Invalid input"
  [(_, out)] -> error ("Unused input: " ++ out)


-- Q1
comment :: Parser ()
comment = do
  symbol "--"
  many (sat (/='\n'))
  -- symbol ['\n']
  return ()

-- Q2
{-
expr ::= expr + expr | term
term ::= term * term | factor
factor ::= ( expr ) | nat
nat ::= 0 | 1 | 2 | ...

2+3+4
            expr
    expr      +       expr
  expr  + expr        term
  term    term        factor
  factor  factor      nat
  nat     nat         4
  2       3

          expr
    expr  +     expr
    term      expr +  expr
    factor    term    term
    nat       factor  factor
    2         nat     nat
              3       4
-}


-- Q3
{-
expr ::= term + expr | term
term ::= factor * term | factor
factor ::= ( expr ) | nat
nat ::= 0 | 1 | 2 | 3 ...

2+3
      expr
  term  +   expr
  factor    term
  nat       factor
  2         nat
            3

2*3*4

        expr
        term
    factor  *       term
    nat        factor * term
    2          nat      factor
               3        nat
                        4

(2+3)+4

          expr
    term    +         expr
    factor            term
    ( expr )          factor
    term  +   expr    nat
    factor    term    4
    nat       factor
    2         nat
              3

-}

-- Q4
{-
expr ::= term (+ expr | e)
term ::= factor (* term | e)
factor ::= ( expr ) | nat
nat ::= 0 | 1 | 2 | 3 | ...

2
    expr
    term
    factor
    nat
    2

expr ::= term + expr | term
term ::= factor * term | factor
factor ::= ( expr ) | nat
nat ::= 0 | 1 | 2 | 3 ...

2
      expr
      term
      factor
      nat
      2
-}


-- Q5
data Expr = E Term | Add Term Expr deriving Show
data Term = T Factor | Mul Factor Term deriving Show
data Factor = F Expr | Val Int deriving Show

expr2 :: Parser Expr
expr2 = do
  t <- term2
  do
    symbol "+"
    e <- expr2
    return (Add t e)
    <|> return (E t)

term2:: Parser Term
term2 = do
  f <- factor2
  do
    symbol "*"
    t <- term2
    return (Mul f t)
    <|> return (T f)

factor2 :: Parser Factor
factor2 = do
  symbol "("
  e <- expr2
  symbol ")"
  return (F e)
  <|> do
    n <- natural
    return (Val n)

-- Q6
{-
expr ::= term (+ expr | - expr | e)
term ::= factor (* term | /term | e)
factor ::= ( expr ) | int
int = ... | -1 | 0 | 1 | ...

-}

expr' :: Parser Int
expr' = do
  t <- term'
  (addP t) <|> (subP t) <|> return t

addP :: Int -> Parser Int
addP t = do
  symbol "+"
  e <- expr'
  return (t+e)

subP :: Int -> Parser Int
subP t = do
  symbol "-"
  e <- expr'
  return (t-e)

term' :: Parser Int
term' = do
  f <- factor'
  (mulP f) <|> (divP f) <|> return f

mulP :: Int -> Parser Int
mulP f = do
  symbol "*"
  t <- term'
  return (f*t)

divP :: Int -> Parser Int
divP f = do
  symbol "/"
  t <- term'
  return (f `div` t)

factor' :: Parser Int
factor' = do
  symbol "("
  e <- expr'
  symbol ")"
  return e
  <|> integer

eval' :: String -> Int
eval' s = case parse expr' s of
  [(x, [])] -> x
  [] -> error "Invalid input"
  [(_, out)] -> error ("Unused input: " ++ out)

-- Q7
{-
expr ::= term (+ expr | - expr | e)
term ::= factor (* term | / term | e)
factor ::= exp (^ factor | e)
exp ::= ( expr ) | int
int = ... | -1 | 0 | 1 | ...
-}

expr'' :: Parser Int
expr'' = do
  t <- term''
  (addP' t) <|> (subP' t) <|> return t

addP' :: Int -> Parser Int
addP' t = do
  symbol "+"
  e <- expr''
  return (t+e)

subP' :: Int -> Parser Int
subP' t = do
  symbol "-"
  e <- expr''
  return (t-e)

term'' :: Parser Int
term'' = do
  f <- factor''
  (mulP' f) <|> (divP' f) <|> return f

mulP' :: Int -> Parser Int
mulP' f = do
  symbol "*"
  t <- term''
  return (f*t)

divP' :: Int -> Parser Int
divP' f = do
  symbol "/"
  t <- term''
  return (f `div` t)

factor'' :: Parser Int
factor'' = do
  e <- exp'
  (do
    symbol "^"
    f <- factor''
    return (e^f)) <|> return e

exp' :: Parser Int
exp' = do
  symbol "("
  e <- expr''
  symbol ")"
  return e
  <|> integer

eval'' :: String -> Int
eval'' s = case parse expr'' s of
  [(x, [])] -> x
  [] -> error "Invalid input"
  [(_, out)] -> error ("Unused input: " ++ out)