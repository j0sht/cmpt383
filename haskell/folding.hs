-- Folding is a general name for a family of related recursive patterns.
-- The essential idea of folding is to take a list and reduce it to
--  a single value.

-- The following functions follow the fold pattern
mysum :: [Int] -> Int
mysum [] = 0
mysum (x:xs) = x + mysum xs

myprod :: [Int] -> Int
myprod [] = 1
myprod (x:xs) = x * myprod xs

-- Notice the similarites of mysum and myprod.
-- The only differences are the name, the value returned for the base case,
--  and a function used in the recursive case.

len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + len xs

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ (myconcat xs)

-- All the functions in the above have the gneral structure of a
--  right-associative fold:
--  - A base case that returns a value such as 0, 1, or []
--  - A recursive case that takes the first element of the list and combines
--    it with the rest of the folded list. The combining function always
--    takes two inputs.
-- General right fold
myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr _ init_val [] = init_val
myfoldr f init_val (x:xs) = f x (myfoldr f init_val xs)

-- With myfoldr (or the built-in foldr) we can rewrite the functions above
mysum2 :: [Int] -> Int
mysum2 = myfoldr (+) 0

myprod2 :: [Int] -> Int
myprod2 = myfoldr (*) 1

len2 :: [a] -> Int
len2 = myfoldr (\_ y -> 1 + y) 0 -- think of y as the accumulator

myconcat2 :: [[a]] -> [a]
myconcat2 = myfoldr (++) []

-- myfoldr is right associative, so myfoldr (+) 0 [2,1,5,2] is evaluated
-- like: 2 + (1 + (5 + (2 + 0)))
-- right-most '+' is evaluated first, then second right-most and so on
--  until the very fist '+' is evaluated

-- A left associative fold is like a right fold, but the expression is
--  instead bracketed from left to right.
myfoldl :: (b -> a -> b) -> b -> [a] -> b
myfoldl f acc [] = acc
myfoldl f acc (x:xs) = myfoldl f (f acc x) xs
-- ex) myfoldl (+) 0 [2,1,5,2] = (((0 + 2) + 1) + 5) + 2

-- A difference between left and right folds is how they interact with
--  Haskell's lazy evaluation. For example, left folds can work with
--  infinite lists in some cases because they fold starting at the beginning
--  of the list and move to the right. Right folds cannot work with
--  infinite lists, because they start at the right end of the list; but
--  infinite lists do not have a right end.

-- NOTE: There is a version of foldl called foldl' that is more efficient.
-- In practice, choice is usually between foldr and foldl'
