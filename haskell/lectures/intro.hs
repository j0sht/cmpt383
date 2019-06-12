inc :: Integer -> Integer -- type signature
inc n = n + 1

len1 :: [a] -> Int
len1 [] = 0
len1 lst = 1 + (len1 (tail lst))

len2 :: [a] -> Int
len2 [] = 0
len2 (_:xs) = 1 + (len2 xs)

-- Using guards
len3 :: [a] -> Int
len3 seq
     | null seq = 0
     | otherwise = 1 + (len3 (tail seq))

len4 :: [a] -> Int
len4 seq = if null seq
           then 0
           else 1 + len4 (tail seq)

plural1 :: String -> String
plural1 a = a ++ ['s']

-- Put '!' at start of String
exclaim :: String -> String
exclaim a = "!" ++ a

-- Curried version
exclaim2 :: String -> String
exclaim2 = (++) "!"

add :: Integer -> Integer -> Integer
add = (+)

-- Point free style
inc2 :: Integer -> Integer
inc2 = add 1

