data Token = Num Double | Inc | Dec | Sqrt | Sin | Cos | Inv | Plus | Mult
  | Minus | Div | PlusAll | MultAll | Dup | Pop | Clear | Swap
  | Error String
  deriving (Eq)

instance Show Token where
  show (Num n) = show n
  show Inc = "inc"
  show Dec = "dec"
  show Sqrt = "sqrt"
  show Sin = "sin"
  show Cos = "cos"
  show Inv = "inv"
  show Plus = "+"
  show Mult = "*"
  show Minus = "-"
  show Div = "/"
  show PlusAll = "+all"
  show MultAll = "*all"
  show Dup = "dup"
  show Pop = "pop"
  show Clear = "clear"
  show Swap = "swap"
  show (Error s) = s

isDigit :: Char -> Bool
isDigit '1' = True
isDigit '2' = True
isDigit '3' = True
isDigit '4' = True
isDigit '5' = True
isDigit '6' = True
isDigit '7' = True
isDigit '8' = True
isDigit '9' = True
isDigit '0' = True
isDigit _ = False

isNumber :: [Char] -> Bool
isNumber s = (numDecimals == 0 || numDecimals == 1) && allDigits
             where numDecimals = length (filter (\x -> x == '.') s)
                   allDigits = all isDigit (filter (\x -> x /= '.') s)


singleArgTokenFunc :: Token -> (Token -> Token)
singleArgTokenFunc Inc = (\(Num a) -> Num (a + 1))
singleArgTokenFunc Dec = (\(Num a) -> Num (a - 1))
singleArgTokenFunc Sqrt = (\(Num a) -> Num (sqrt(a)))
singleArgTokenFunc Sin = (\(Num a) -> Num (sin(a)))
singleArgTokenFunc Cos = (\(Num a) -> Num (cos(a)))
singleArgTokenFunc Inv = (\(Num a) -> Num (1 / a))

doubleArgTokenFunc :: Token -> (Token -> Token -> Token)
doubleArgTokenFunc Plus = (\(Num a) (Num b) -> Num (a + b))
doubleArgTokenFunc Mult = (\(Num a) (Num b) -> Num (a * b))
doubleArgTokenFunc Minus = (\(Num a) (Num b) -> Num (a - b))
doubleArgTokenFunc Div = (\(Num a) (Num b) -> Num (a / b))

stackArgTokenFunc :: Token -> ([Token] -> Token)
stackArgTokenFunc PlusAll = (\(tokens) ->
                               foldl
                               (\(Num acc) (Num x) -> Num (acc + x))
                               (Num 0)
                               tokens)
stackArgTokenFunc MultAll = (\(tokens) ->
                               foldl
                               (\(Num acc) (Num x) -> Num (acc * x))
                               (Num 1)
                               tokens)

stackManipTokenFunc :: Token -> ([Token] -> [Token])
stackManipTokenFunc Dup = (\(x:xs) -> x:x:xs)
stackManipTokenFunc Pop = (\(x:xs) -> xs)
stackManipTokenFunc Clear = (\(tokens) -> [])
stackManipTokenFunc Swap = (\(x1:x2:xs) -> x2:x1:xs)

tokenize :: String -> Token
tokenize x
  | x == "inc" = Inc
  | x == "dec" = Dec
  | x == "sqrt" = Sqrt
  | x == "sin" = Sin
  | x == "cos" = Cos
  | x == "inv" = Inv
  | x == "+" = Plus
  | x == "-" = Minus
  | x == "*" = Mult
  | x == "/" = Div
  | x == "+all" = PlusAll
  | x == "*all" = MultAll
  | x == "dup" = Dup
  | x == "pop" = Pop
  | x == "clear" = Clear
  | x == "swap" = Swap
  | isNumber x = Num (read x :: Double)
  | otherwise = Error x

isSingleArgOp :: Token -> Bool
isSingleArgOp x
  | x == Inc = True
  | x == Dec = True
  | x == Sqrt = True
  | x == Sin = True
  | x == Cos = True
  | x == Inv = True
  | otherwise = False

isDoubleArgOp :: Token -> Bool
isDoubleArgOp x
  | x == Plus = True
  | x == Mult = True
  | x == Minus = True
  | x == Div = True
  | otherwise = False

isStackArgOp :: Token -> Bool
isStackArgOp x
  | x == PlusAll = True
  | x == MultAll = True
  | otherwise = False

isStackManipOp :: Token -> Bool
isStackManipOp x
  | x == Dup = True
  | x == Pop = True
  | x == Clear = True
  | x == Swap = True
  | otherwise = False

isNumberToken :: Token -> Bool
isNumberToken (Num _) = True
isNumberToken _ = False


calc :: String -> String
calc s = processStack (map tokenize (words s))

reducer :: [Token] -> Token -> [Token]
reducer acc x
  | isNumberToken x = x:acc
  | isSingleArgOp x = if (length acc) > 0
                      then ((singleArgTokenFunc x) (head acc)) : (tail acc)
                      else (Error ((show x) ++ ": empty stack")) : []
  | isDoubleArgOp x = if (length acc) > 1
                      then ((doubleArgTokenFunc x)
                            (head (tail acc)) (head acc)) :
                           (tail (tail acc))
                      else (Error ((show x) ++ ": not enough args")) : []
  | isStackArgOp x = ((stackArgTokenFunc x) acc) : []
  | isStackManipOp x = (stackManipTokenFunc x) acc
  | otherwise = acc
  

processStack :: [Token] -> String
processStack tokens = if (length result) > 0
                      then show (head result)
                      else "empty stack"
  where result = foldl reducer [] tokens
