equal :: Eq a => [a] -> [a] -> Bool
equal [] []         = True
equal [] _          = False
equal _ []          = False
equal (a:as) (b:bs) = if a == b then equal as bs else False
