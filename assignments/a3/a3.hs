-- 1)
-- snoc x lst returns a new list with x added to end of lst
-- requirement: use only basic recursion
snoc :: a -> [a] -> [a]
snoc x []     = [x]
snoc x (y:ys) = y : snoc x ys

-- 2)
-- myappend a b adds b to the end of a
-- requirement: don't use ++ or concat
myappend :: [a] -> [a] -> [a]
myappend [] b     = b
myappend (a:as) b = a : myappend as b

-- 3)
-- myreverse reverses the items of a list
-- requirement: don't use any non-trivial functions unless custom written
myreverse :: [a] -> [a]
myreverse xs = myfoldl (\acc x -> x:acc) [] xs
-- used for myreverse
myfoldl :: (a -> b -> a) -> a -> [b] -> a
myfoldl fn acc []     = acc
myfoldl fn acc (x:xs) = myfoldl fn (fn acc x) xs

-- 4)
-- count_emirps returns the number of emirps less than, or equal to, n
-- (emirp --> a prime number that is a different prime when its
--            digits are reversed)
-- First few emirps are: 13, 17, 31, 37, 71, 73
count_emirps n = myfoldl (\acc x -> if is_emirp x
                                    then acc+1
                                    else acc) 0 [1..n]

-- CITATION: smallest_divisor and is_prime is borrowed from:
-- http://www.sfu.ca/~tjd/383summer2019/haskell_functions_lhs.html
-- Found under "Example: Finding Primes"
smallest_divisor :: Integer -> Integer
smallest_divisor n
  | n < 0     = error "n must be >= 0"
  | n == 0    = 0
  | n == 1    = 1
  | otherwise = head (dropWhile (\x -> n `mod` x /= 0) [2..n])

is_prime :: Integer -> Bool
is_prime n
  | n < 2     = False
  | otherwise = (smallest_divisor n) == n

is_emirp :: Integer -> Bool
is_emirp n = is_prime reverse_n && is_prime n && reverse_n /= n
             where reverse_n = (read (myreverse (show n)) :: Integer)

-- 5)
-- biggest_sum takes a list of one or more integer lists as input
-- and returns the list with the greatest sum
-- can assume list passed to biggest_sum is non-empty
biggest_sum :: [[Int]] -> [Int]
biggest_sum nums = myfoldl (\acc lst -> if sum(lst) > sum(acc)
                                        then lst
                                        else acc) (head nums) (tail nums)

-- 6) Write a function called greatest, which returns the item in seq
--    that maximizes function d
-- If more than one item maximizes f, then greatest f returns first one
greatest :: (a -> Int) -> [a] -> a
greatest f xs = myfoldl (\acc lst -> if f(lst) > f(acc)
                                     then lst
                                     else acc) (head xs) (tail xs)

-- 7)
-- is_bit x returns True when x is 0 or 1, and False otherwise
-- assume x is of type Int, and the type of the returned value is Bool
-- Include the most general type signature
is_bit :: Int -> Bool
is_bit x = if (x == 0) || (x == 1)
           then True
           else False

-- 8)
-- Write a function called flip_bit x that returns 1 if x is 0, and 0 if
-- x is 1. If x is not a bit, then call error msg, where msg is a helpful
-- error message string.
-- Assume x is of type Int and the type of the returned value is also Int
-- Include the most general type signature
flip_bit :: Int -> Int
flip_bit x
  | x == 1    = 0
  | x == 0    = 1
  | otherwise = (error "Not a bit (0 or 1)")

-- 9a) Write a function called is_bit_seq1 x that returns True if x is
--     the empty list, or if it contains only bits (as determined by
--     is_bit). It should return False otherwise. Use guards
is_bit_seq1 :: [Int] -> Bool
is_bit_seq1 x
  | x == []   = True
  | otherwise = is_bit (head x) && is_bit_seq1 (tail x)

-- 9b) is_bit_seq2 (same as a but use if-then-else)
is_bit_seq2 :: [Int] -> Bool
is_bit_seq2 []     = True
is_bit_seq2 (x:xs) = if is_bit x
                     then is_bit_seq2 xs
                     else False

-- 9c) is_bit_seq3 (same as a & b, but use list comprehension)
is_bit_seq3 :: [Int] -> Bool
is_bit_seq3 ns = not (elem False [ is_bit n | n <- ns ])

-- 10a) Write a function called invert_bits1 x that returns a sequence
--      of bits that is the same as x, except 0s become 1s and 1s become
--      0s. Use basic recursion
invert_bits1 :: [Int] -> [Int]
invert_bits1 []     = []
invert_bits1 (n:ns) = flip_bit n : invert_bits1 ns

-- 10b) Same as a, but implement using map
invert_bits2 :: [Int] -> [Int]
invert_bits2 bits = map (\n -> flip_bit n) bits

-- 10c) Same as a and b, but implement using list comprehension
invert_bits3 :: [Int] -> [Int]
invert_bits3 bits = [flip_bit n | n <- bits]

-- 11) Write a function called bit_count x that returns a pair of values
--     indicating the number of 0s and 1s in x.
--     Ex) bit_count [1,1,0,1] returns (1, 3) -> one 0, three 1s
bit_count :: [Int] -> (Int, Int)
bit_count x = myfoldl (\pair n -> if n == 0
                                  then ((fst pair) + 1, snd pair)
                                  else (fst pair, (snd pair) + 1)) (0,0) x

-- 12) all_basic_bit_seqs n returns a list of all bit sequences of length n
--     Order of the sequences doesn't matter. If n < 1, return empty list.
--     Assum n is an Int, and returned value is a list of Int lists
all_basic_bit_seqs :: Int -> [[Int]]
all_basic_bit_seqs n
  | n < 1 = []
  | otherwise = map myreverse
                [(pad_with_zeros (n-len) bits) | x <- [0..(2^n)-1],
                 let bits = (binary x), let len = (length bits)]

binary :: Int -> [Int]
binary n
  | n <= 0    = []
  | otherwise = (n `mod` 2) : binary (n `quot` 2)

pad_with_zeros :: Int -> [Int] -> [Int]
pad_with_zeros n lst
  | n <= 0    = lst
  | otherwise = (myappend lst (replicate n 0))

-- 13) Implement toList :: [a] -> List a, which converts a regular
--     Haskell list to a List a
data List a = Empty | Cons a (List a)
  deriving Show

toList :: [a] -> List a
toList []     = Empty
toList (x:xs) = Cons x (toList xs)

-- 14) Implement toHaskellList :: List a -> [a], which converts a List
--     a to a regular Haskell list
toHaskellList :: List a -> [a]
toHaskellList Empty        = []
toHaskellList (Cons a as)  = a : toHaskellList as

-- 15) Implement append A B, that returns a new List a that consists
--     of all the elements of A followed by all the elements of B
append :: List a -> List a -> List a
append Empty b          = b
append (Cons a as) b    = Cons a (append as b)

-- 16) Implement the function removeAll f L that returns a List a that
--     is the same as L but all items satisfying f have been removed.
--     f is a predicate function of type a -> Bool and L has type List a
removeAll :: (a -> Bool) -> List a -> List a
removeAll f Empty       = Empty
removeAll f (Cons a as) = if f a
                          then removeAll f as
                          else Cons a (removeAll f as)

-- 17) Implement sort L, where L has type List a, that returns a new List
--     a that is a sorted version of L (in ascending order). User either
--     quicksort or mergesort.
sort :: Ord a => List a -> List a
sort Empty       = Empty
sort (Cons a as) = smalls `append` Cons a Empty `append` bigs
                   where smalls = sort (removeAll (\x -> x > a) as)
                         bigs   = sort (removeAll (\x -> x <= a) as)
