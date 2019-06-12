-- RGB Color
in_range :: Int -> Bool
in_range n = 0 <= n && n <= 255

is_rgb :: (Int, Int, Int) -> Bool
is_rgb (r, g, b) = (in_range r) && (in_range g) && (in_range b)

is_gray :: (Int, Int, Int) -> Bool
is_gray (r, g, b) = (r == g && g == b)

invert_rgb :: (Int, Int, Int) -> Bool
invert_rgb (r, g, b) = (255 - r, 255 - g, 255 - b)

brightness :: (Int, Int, Int) -> Float
brightness (r, g, b) = ((fromIntegral r) +
                        (fromIntegral g) +
                        (fromIntegral b)) / 3.0

-- dist using let/in
dist :: (Int, Int, Int) -> (Int, Int, Int) -> Float
dist (r1,g1,b1) (r2,g2,b2) = let dr = fromIntegral (r1 - r2)
                                 dg = fromIntegral (g1 - g2)
                                 db = fromIntegral (b1 - b2)
                             in sqrt (dr^2 + db^2 + dg^2)

-- dist using where
dist2 :: (Int, Int, Int) -> (Int, Int, Int) -> Float
dist2 (r1, g1, b1) (r2, g2, b2) = sqrt (dr^2 + dg^2 + db^2)
                                  where dr = fromIntegral (r1 - r2)
                                        dg = fromIntegral (g1 - g2)
                                        db = fromIntegral (b1 - b2)

-- constraint lo hi x; uses guards instead of patterns
constrain :: Int -> Int -> Int -> Int
constrain lo hi x
  | x < lo    = lo
  | hi < x    = hi
  | otherwise = x

-- legal is a local function (that uses currying)
constrain_rgb :: (Int, Int, Int) -> (Int, Int, Int)
constrain_rgb (r, g, b) = (legal r, legal g, legal b)
                          where legal = constrain 0 255

-- add two RGB colors, constraining values greater than 255
simple_add :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
simple_add (r1, g1, b1) (r2, g2, b2) = constrain_rgb (r1+r2
                                                      g1+g2
                                                      b1+b2)
-- Convert a string color name into an RGB triple
to_rgb :: String -> (Int, Int, Int)
to_rgb "red"   = (255, 0, 0)
to_rgb "green" = (0, 255, 0)
to_rgb "blue"  = (0, 0, 255)
to_rgb "white" = (255, 255, 255)
to_rgb "black" = (0, 0, 0)
to_rgb _       = error "Unknown color"

-- Convert an RGB Triple into a string name
to_str :: (Int, Int, Int) -> String
to_str (255, 0, 0)     = "red"
to_str (0, 255, 0)     = "green"
to_str (0, 0, 255)     = "blue"
to_str (255, 255, 255) = "white"
to_str (0, 0, 0)       = "black"
to_str _               = error "Uknown RGB triple"
