head' :: [a] -> a
head' (x : _) =  x

tail' :: [a] -> [a]
tail' [] = []
tail' (_ : x) =  x

take' :: Int -> [a] -> [a]
take' n [] = []
take' n (x : xs) = if n > 0
				   then (x : (take' (n - 1) xs))
				   else (if n == 0
						then []
						else [x])

drop' :: Int -> [a] -> [a]
drop' n [] = []
drop' n (x : xs) = if n > 1
				   then drop' (n - 1) xs
				   else xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = [x | x <- xs, f x == True]

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f z [] = z
foldl' f z (l : ls) = foldl' f (f z l) ls

concat' :: [a] -> [a] -> [a]
concat' a [] = a
concat' [] a = a
concat' (x : xs) ys = (x : concat' xs ys)

quickSort' :: Ord a => [a] -> [a]
quickSort' [a] = [a]
quickSort' [] = []
quickSort' (x : xs) = concat' (concat' (quickSort' (filter' (< x) xs)) ((:) x (filter' (== x) xs))) (quickSort' (filter' (> x) xs)) 

