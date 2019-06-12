nand :: Bool -> Bool -> Bool
nand False False = True
nand False True = True
nand True False = True
nand True True = False

-- sum numbers 1 through N
mysum1 :: Integer -> Integer
mysum1 n = sum [1..n]

mysum2 :: Int -> Int
mysum2 0 = 0
mysum2 n = n + (mysum2 (n - 1))

mysum3 :: Int -> Int
mysum3 n
       | n < 0     = error "n must be >= 0"
       | n == 0    = 0
       | otherwise = n + mysum3 (n - 1)
