sumPair :: (Num a, Eq a) => [a] -> a -> [a]
sumPair []  _ = []
sumPair [_] _ = []
sumPair (x : xs) s | flip elem xs $ s - x = [x, s - x]
sumPair (_ : xs) s = sumPair xs s
