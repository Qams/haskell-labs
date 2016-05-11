-- some functions with using fold

sum' a = foldl (+) 0 a

product' a = foldl (*) 1 a

reverse' a = foldl (\acc x -> x : acc) [] a

and' a = foldl1 (\x xs -> x && xs) a

or' a = foldl1 (\x xs -> x || xs) a

head' a = foldr1 (\x _ -> x) a 

last' a = foldl1 (\xs x -> x) a
