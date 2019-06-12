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
