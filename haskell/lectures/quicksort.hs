quicksort :: Ord a => [a] -> [a]
quicksort [] = []
-- You can read n <- xs, n <= x as:
-- n is an element of xs and n is less than or equal to x
quicksort (x:xs) = smalls ++ [x] ++ bigs
                   where smalls = quicksort [n | n <- xs, n <= x]
                         bigs   = quicksort [n | n <- xs, n > x]
                         
