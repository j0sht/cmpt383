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
