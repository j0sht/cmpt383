-- Season is an example of a user-created data type
-- Season is the name of the type, and it's four possible values are
--  Winter, Spring, Summer and Fall. It is similar to an enumerated type
--  in a language like C++ or Java.
-- The Eq in the deriving clause lets us use == and /= with Season.
-- Show lets Season values be converted to strings so that, for instance,
--  they can be printed in the interpreter.
data Season = Winter | Spring | Summer | Fall
  deriving (Eq, Show)

-- Example of using Season in a function
seasonCode :: Season -> Int
seasonCode Winter = 1
seasonCode Spring = 2
seasonCode Summer = 4
seasonCode Fall   = 6

-- BasicShape is a user-created type that represents circles and rectangles
data BasicShape = BasicCircle Float | BasicRect Float Float
-- Circle is a data constructor for BasicShape, and it takes one Float as
--  input (the radius), while Rect is a data constructor that takes two
--  floats as input (width and height)

-- Given a BasicShape value, we don't know if its a circle or a rectangle
--  so we have to check for both cases.
basicArea :: BasicShape -> Float
basicArea (BasicCircle r) = pi * r^2
basicArea (BasicRect w h) = w * h
-- Each equation in basicArea defines an area formula for the possible shapes

basicCircum :: BasicShape -> Float
basicCircum (BasicCircle r) = 2 * pi * r
basicCircum (BasicRect w h) = 2 * (w + h)

-- More sophisticated Shape with Points
data Point = Point Float Float
  deriving (Show, Eq)

dist :: Point -> Point -> Float
dist (Point x1 y1) (Point x2 y2) = sqrt (dx^2 + dy^2)
                                   where dx = x1-x2
                                         dy = y1-y2

-- dist_to_origin is a curried function that calculates the distance from
-- a point to the origin. Notice there is no need to write a variable for
-- dist_to_origin.
dist_to_origin :: Point -> Float
dist_to_origin = dist (Point 0 0)

data Shape = Circle Point Float | Rect Point Float Float
  deriving (Show, Eq)

-- We don't need the point to calculate a Shape's area (so we can use _)
area :: Shape -> Float
area (Circle _ r) = 2 * pi * r
area (Rect _ w h) = 2 * (w + h)

location :: Shape -> Point
location (Circle (Point x y) _) = (Point x y)
location (Rect (Point x y) _)   = (Point x y)

-- shapeDist calculates the distance between two shapes
shapeDist :: Shape -> Shape -> Float
shapeDist s1 s2 = dist (location s1) (location s2)
