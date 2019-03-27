-- COMP90048 Declarative Programming Assignment One
-- Author: Peiyong Wang 955986
module Assignment1 (subst, interleave, unroll) where
-- Additional function len takes a list and return the length of the list
len :: Num a1 => [a2] -> a1
len [] = 0
len (x:xs) = 1 + len(xs)

-- subst takes two values and a list, and replaces every occurrence of the first value with the second in the list.
subst :: Eq t => t -> t -> [t] -> [t]
-- ori: original value
-- rep: value to replace ori
subst ori rep (x:xs) = 
    if x == ori
        then [rep] ++ (subst ori rep xs)
    else
        [x] ++ (subst ori rep xs)
subst ori rep [] = []

-- interleave takes two lists and returns the interleaving of the two arguments. That, the result is a list in which the first, third, fifth . . . elements come fromt the first argument and the second, fourth, sixth . . . come from second. If either argument is shorter than the other, the excess elements of the longer comprise the end of the resulting list.
interleave :: [t] -> [t] -> [t]
interleave [] a = a
interleave b [] = b
interleave (x:xs) (y:ys) = 
    x:y:(interleave xs ys)

-- unroll takes a list and an integer and constructs a list of the specified length made up by “unrolling” the input list as many times as needed to construct a list of that length. That is, the output consists of the input list repeated as many times as necessary to have the specified length.
unroll :: Int -> [a] -> [a]
unroll 0 _ = []
unroll _ [] = []
unroll n (x:xs) =
    if n > len(x:xs) 
        then unroll n ((x:xs)++(x:xs))
    else
        if n == len(x:xs)
            then (x:xs)
        else
            if n >= 1 && n < len(x:xs)
                then [x] ++ unroll (n-1) xs
            else
                []
