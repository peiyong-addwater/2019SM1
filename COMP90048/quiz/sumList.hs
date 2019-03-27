sumList :: Num a => [a] -> a
sumList [] =0
sumList (x:xs) = x + sumList(xs)

