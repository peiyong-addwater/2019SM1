--Question  4
len1::[t]->Int
len1[] = 0
len1(x:xs) = 1+len1 xs

--Q 5
xorFunc::Bool->Bool->Bool
xorFunc a b = (a && (not b))||((not a)&&b)
--Q6
append::[t]->[t]->[t]
append [] a = a
append (x:xs) a = x:(append xs a)
--Q7
haReverse::[t]->[t]
haReverse []=[]
haReverse x:xs = (haReverse xs)++(haReverse [x])

