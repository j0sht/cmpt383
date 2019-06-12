inc :: Integer -> Integer
inc n = n + 1

-- calculate the length of the hypotenuse of a right-triangle
-- with leg lengths a and b
hyp a b = sqrt (a*a + b*b)

-- calculate the distance between points (x,y) and (a,b)
dist x y a b = hyp (x - a) (y - b)

len :: [a] -> Integer -- type signature [a] is a list of any type
len [] = 0 -- base case
len(x:xs) = 1 + len xs -- recursive case
