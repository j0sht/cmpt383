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
isDigit x = x `elem` ['0'..'9']

isNumber :: [Char] -> Bool
isNumber s = (numDecimals == 0 || numDecimals == 1) && allDigits
             where numDecimals = length (filter (\x -> x == '.') s)
                   allDigits = all isDigit (filter (\x -> x /= '.' &&
                                                     x /= '-' && x /= '('
                                                     && x /= ')')
                                            s)

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
stackManipTokenFunc Clear = (\(_) -> [])
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
  | otherwise = Error ("Invalid token: " ++ x)

isSingleArgOp :: Token -> Bool
isSingleArgOp Inc = True
isSingleArgOp Dec = True
isSingleArgOp Sqrt = True
isSingleArgOp Sin = True
isSingleArgOp Cos = True
isSingleArgOp Inv = True
isSingleArgOp _ = False

isDoubleArgOp :: Token -> Bool
isDoubleArgOp Plus = True
isDoubleArgOp Mult = True
isDoubleArgOp Minus = True
isDoubleArgOp Div = True
isDoubleArgOp _  = False

isStackArgOp :: Token -> Bool
isStackArgOp PlusAll = True
isStackArgOp MultAll = True
isStackArgOp _ = False

isDupOrPop :: Token -> Bool
isDupOrPop Dup = True
isDupOrPop Pop = True
isDupOrPop _ = False

isClear :: Token -> Bool
isClear Clear = True
isClear _ = False

isSwap :: Token -> Bool
isSwap Swap = True
isSwap _ = False

isNumberToken :: Token -> Bool
isNumberToken (Num _) = True
isNumberToken _ = False

isError :: Token -> Bool
isError (Error _) = True
isError _ = False

reducer :: [Token] -> Token -> [Token]
reducer acc x
  | isNumberToken x = if notEmptyNotErr
                      then x:acc
                      else if len == 0
                           then x:acc
                           else acc
  | isSingleArgOp x = if len > 0 && (not err) && oneNum
                      then ((singleArgTokenFunc x) (head acc)) : (tail acc)
                      else if empty
                           then (Error ((show x) ++ ": empty stack")) : []
                           else acc
  | isDoubleArgOp x = if len > 1 && (not err) && twoNums
                      then ((doubleArgTokenFunc x)
                            (head (tail acc)) (head acc)) :
                           (tail (tail acc))
                      else if empty || notEmptyNotErr
                           then (Error ((show x) ++ ": not enough args"))
                                : []
                           else acc
  | isStackArgOp x = if len > 0 && (not err)
                     then ((stackArgTokenFunc x) acc) : []
                     else if empty
                          then (Error ((show x) ++ ": empty stack")) : []
                          else acc
  | isDupOrPop x = if len > 0 && (not err)
                   then (stackManipTokenFunc x) acc
                   else if empty
                        then (Error ((show x) ++ ": empty stack")) : []
                        else acc
  | isSwap x = if len > 1 && (not err)
               then (stackManipTokenFunc x) acc
               else if empty
                    then (Error ((show x) ++ ": not enough args")) : []
                    else acc
  | isClear x = if notEmptyNotErr
                then (stackManipTokenFunc x) acc
                else acc
  | isError x = if empty || notEmptyNotErr
                then x:[]
                else acc
  | otherwise = acc
  where len = length acc
        empty = len == 0
        err = isError (head acc)
        notEmptyNotErr = (not empty) && (not err)
        oneNum = isNumberToken (head acc)
        twoNums = isNumberToken (head acc) &&
                  isNumberToken (head (tail acc))

processStack :: [Token] -> String
processStack tokens = if (length result) > 0
                      then show (head result)
                      else "empty stack"
  where result = foldl reducer [] tokens

-- calc and calcStack
calc :: String -> String
calc s = processStack (map tokenize (words s))

calcStack :: String -> String
calcStack s = "Top of stack --> " ++ (show result)
  where result = reverse (map tokenize (words s))
