import Prelude hiding (last)

-- Lab3 Бєляєвої Юлії 1 гр

--1 
listSum :: [Int] -> ([Int] -> [Int])
listSum [] [] = []
listSum [] (y:ys) = (y:ys)
listSum (x:xs)  [] = (x:xs) 
listSum (x:xs) (y:ys) = ((x+y) : listSum xs ys)

--2 
--oddEven - міняє місцями сусідні парні і непарні елементи в заданому списку.
oddEven [] = []
oddEven (x:xs) = reverse(take 2 (x:xs)) ++ oddEven(drop 2 (x:xs))

--3 
position y (x:xs) 
	|not(y `elem` (x:xs)) = error "Not an element."
	|otherwise = length(x:xs) - formula y (x:xs)
		where formula y (x:xs) = if (y == head(x:xs)) then length(x:xs) else formula y (drop 1 (x:xs))

--4 
frequency [] = [[]]
frequency [x] = [[x,1]]
frequency list = [(head (list)), chastota (head (list)) list]: frequency (delete (head (list)) (tail (list)))

chastota y list 
        |(y `elem` list) = 1 + (chastota y (drop ((position y list)+1) list ))
        |otherwise = 0


delete y list
	| list == [] = []
	| head list == y = delete y (tail list)
	| otherwise = head list : delete y (tail list)

--5 
--set - повертає список, котрий містить всі елементи початкового списку без дублікатів.
set [] = []
set [x] = [x]
set (x:xs) 
	|(chastota x (x:xs) > 1) = x : set (delete x xs)
	|otherwise = x: set xs

--6
--union - повертає список – об»єднання двох списків без дублікатів.
union (x:xs) (y:ys) = set ((x:xs) ++ (y:ys))

--7
--intersection - повертає список – перетин двох списків без дублікатів.
intersection (x:xs) (y:ys) = set(let el = [b | b <- (y:ys),  b `elem` el] in [a | a <- (x:xs), a `elem` (y:ys)])



