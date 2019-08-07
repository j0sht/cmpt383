-- Haskell composition is based on the idea of function composition in math
f :: Int -> Int
f n = n + 1

g :: Int -> Int
g n = 2*n - 1

-- Haskell mirrors the notation of f ∘ g
h = f . g -- h is the composition of f and g

-- The type of . is
-- (.) :: (b -> c) -> (a -> b) -> a -> c

-- The dot operator lets us define functions more compactly
root1 :: Float -> Float
root1 x = sqrt (abs (negate x))

root2 :: Float -> Float
root2 = sqrt . abs . negate -- point free style

-- Point free style means that the input parameter is not written explicitly
mylast = head . reverse     -- point free style

-- The difference between function composition and regular function
--  application can sometimes seem confusing.

-- Composition:
-- f (g (h x)) == (f . g . h) x

-- Application:
-- f g h x == ((f g) h) x
-- A function application like f g h x is a call to the function f, and g,
--  h, and x are the parameters passed to it. The parameters are passed in
--  the order they are given, i.e., first g is passed to f, then h, and
--  finally x. Thus, function application is left-associative.
