import Prelude hiding (length, (++), last)

-- Lab2 Бєляєвої Юлії 1 гр

-- 1.1

paired_numbers :: Int -> [Int]
paired_numbers 0 = []
paired_numbers(n) = take n (zipWith ( * ) [2, 2 ..] [1..])

--1.2

powerTwo :: Integer -> [Integer]
powerTwo 0 = []
powerTwo n = (2 ^ n) : powerTwo (n - 1)

--1.3

t_Ferma_list :: Double -> [Double]
t_Ferma 1 = 1
t_Ferma n = (1/2)*n*(n+1)
--t_Ferma n = n + t_Ferma (n - 1)
t_Ferma_list (n) = map t_Ferma [1 .. n]

--1.4

p_Ferma_list :: Double -> [Double]
p_Ferma 1 = 1
p_Ferma n = (1/6)*n*(n+1)*(2*n+1)
p_Ferma_list (n) = map p_Ferma [1 .. n]

--нескінченний список, починаючи з n  
numbersFrom :: Int -> [Int]
numbersFrom n = n : (numbersFrom (n+1))

--2.1
fifth_degree :: [Int]
fifth_degree = map (^5) (numbersFrom 1)

--2.2
--натуральних степенів числа п»ять
degree_of_fifth :: [Int]
degree_of_fifth = 5 : zipWith ( ^ ) [5, 5 ..] [2, 3 ..] 


--2.3
--другої супер-степені натуральних чисел
syper_degree_two :: [Int]
syper_degree_two = 1: zipWith ( ^ ) [2, 3 ..] [2, 3 ..]

--2.4
--третьої супер-степені натуральних чисел 
syper_degree_three :: [Int]
syper_degree_three = 1: zipWith ( ^ ) [2, 3 ..] (zipWith ( ^ ) [2, 3 ..] [2, 3 ..])

--3
--Написати функцію, що обраховує суму ряду Sum(3^k:k=1..n)

summa :: Int -> Int
summa (n) = foldl (+) 0 (zipWith ( ^ ) [3, 3 ..] [1 ..n])

--4.1
--Написати функцію, що обраховує суму ряду Sum(F(m,k):k=1..n), де F(m,k) = m^k

summa2 :: Int -> (Int -> Int)
summa2 m n = foldl (+) 0 (zipWith ( ^ ) [m, m ..] [1 ..n])

--4.2
----Написати функцію, що обраховує суму ряду Sum(F(m,k):k=1..n), де F(m,k) = m^k / k!
summa3 n m = sum ( [ (m^k) / (product [1..k])| k<-[1..n] ] ) 



