ftoc :: Double -> Double
ftoc f = (5/9)*(f-32)
quadRoots :: Double->Double->Double->[Double]
quadRoots 0 0 _ = error "Not a equation"
quadRoots 0 b c = [-c/b]
quadRoots a b c
    | r < 0 = error "No real solution"
    | r == 0 = [tp]
    | r > 0 = [tp+temp, tp-temp]
    where r = b*b -4*a*c
          temp = sqrt(r)/(2*a)
          tp = -b/(2*a)


data Tree k v = Leaf | Node k v (Tree k v) (Tree k v) deriving (Eq, Show)
same_shape :: Tree a b -> Tree c d -> Bool
same_shape Leaf Leaf = True
same_shape Leaf (Node _ _ _ _) = False
same_shape (Node _ _ _ _) Leaf = False
same_shape (Node _ _ l1 r1) (Node _ _ l2 r2) = (same_shape l1 l2) && (same_shape r1 r2)

data Expression = Var Variable | Num Integer | Plus Expression Expression | Minus Expression Expression | Times Expression Expression | Div Expression Expression
data Variable = A|B
eval :: Integer -> Integer -> Expression -> Integer
eval a b (Num n) = n
eval a b (Var A) = a
eval a b (Var B) = b
eval a b (Plus expr1 expr2) = (eval a b expr1) + (eval a b expr2)
eval a b (Minus expr1 expr2) = (eval a b expr1) - (eval a b expr2)
eval a b (Times expr1 expr2) = (eval a b expr1) * (eval a b expr2)
eval a b (Div expr1 expr2) = (eval a b expr1) `div` (eval a b expr2)
