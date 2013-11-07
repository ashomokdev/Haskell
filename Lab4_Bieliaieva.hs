import Prelude hiding (last)

-- Lab4 Бєляєвої Юлії 1 гр

--1 
sum1 :: (Num a) => [a] -> a 
sum1 [] = 0
sum1 (x:xs) = x + sum1 (xs)

--2
sum2 :: (Num a) => [a] -> a  
sum2 = foldl (+) 0  

--3 
product1:: (Num a) => [a] -> a 
product1 [] = 0
product1 xs =  foldr (\acc x -> acc * x) 1 xs 

--4
concatination :: (Eq a) => [a] -> [a] -> [a]
xs `concatination` ys
	|xs == [] = ys
	|otherwise = foldr (\ys xs -> ys:xs) ys xs  

--5
sort :: [Integer] -> [Integer]
sort x = foldr insert [] x
insert x [] = [x]
insert x (y:ys) = if x <= y then x : y : ys else y : insert x ys

--6
map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 _ [] _ = []
map2 _ _ [] = []
map2 f (x:xs) (y:ys) = f x y : map2 f xs ys

map2' :: (a -> b -> c) -> [a] -> [b] -> [c]
map2' f = foldr cons nil
	where
	nil           _      = [               ]
	cons a match' [    ] = [               ]
	cons a match' (b:bs) = f a b : match' bs

--7
factorials = 1:map2  ( * ) factorials [2..]

--8
primeNums = 2 : [n | n <- [3..], isPrime n]
 
isPrime n = foldr (\p r-> p*p>n || (rem n p /= 0 && r)) True primeNums




 
