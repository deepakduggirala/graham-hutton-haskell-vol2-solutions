--Q1
sumSquares = sum [ x*x | x <- [1..100] ]

--Q2
grid m n = [ (x,y) | x <- [0..m], y <- [0..n] ]

--Q3
square n = [ (x,y) | (x,y) <- grid n n, x /= y ]

--Q4
replicate' n x = [x | _ <- [1..n]]

--Q5
pyths n = [ (x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x*x + y*y == z*z ]

--Q6
factors n = [ x | x <- [1..n], n `mod` x == 0 ]
perfects n = [ x | x <- [1..n], (sum . init . factors $ x) == x]

--Q7
orig = [ (x,y) | x <- [1,2], y <- [3,4] ]
modified = concat [ [(x,y) | y <- [3,4]] | x <- [1,2] ]

--Q9
scalarproduct xs ys = sum [ x*y | (x,y) <- zip xs ys]