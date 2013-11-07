-- Lab1 Бєляєвої Юлії 1 гр
import Prelude hiding (length, (++), map)

-- 1. Написати функцію add_and_double, котра додає два числа і потім подвоює результат

add_and_double :: Int -> (Int -> Int)
add_and_double x y = (x+y)*2


-- 2. Написати інфіксний  оператор +*, котрий робить теж сааме що і  add_and_double. 
-- Визначити його в термінах add_and_double, використовуючи нотацію з літерою (`).

(+*) :: Int -> (Int -> Int)
x+*y = x `add_and_double` y


-- 3. Написати функцію solve_quadratic_equation, котра має три аргумента (a, b, c), 
-- що є коефіцієнти квадратного рівняння ax2+bx+c = 0. a, b, c і x повинні мати тип Double. 
-- Вихід повинене бути кортеж, що містить два корня. Не зв»язуйтеся з комплексними числами; 
-- якщо ви застосуєте функцію sqrt до від»ємного числа, ви одержите NaN (Not a Number). 
-- Використайте let або where вираз, щоб визначити квадратний корінь з дискримінанту (sqrt(b**2 – 4*a*c)).

solve_quadratic_equation :: (Double, Double, Double) -> (Double, Double)
solve_quadratic_equation (a,b,c) = if d < 0 then error "No roots" else (x1, x2)
  where x1 = (-b + sd) / (2 * a)
	x2 = (-b - sd) / (2 * a)
	d  = b * b - 4 * a * c
	sd = sqrt d

-- 4. Написати функцію first_n, котра бере Int (але не Integer!)  значення (n) і повертає список перших n Int, починаючи з 1. В вашому рішенні використайте нескінченний список і функцію take з Prelude (подивіться документацію по функціям Prelude = GHC Documentation+Libraries+Prelude). Ми використовуємо Int замісто Integer, тому що перший аргумент функції take – тип Int.

first_n :: Int -> [Int] 
first_n (n) = take n [1 ..]

-- 5. Перепишіть first_n на нову функцію first_n_integers, котра бере аргумент Integer. Використайте це, визначивши локальну допоміжну функцію take_integer,  котра має Integer – перший аргумент і список Integer – другий аргумент. Використайте let або where вираз для визначення допоміжної локальної функції. Відзначимо, що ви можете використати функцію error,  що сигналізує про помилку. Перевірте, що перший аргумент take_integer >= 0 (охорона-зразок працює добре) і що другий аргумент не порожній список, якщо її перший аргумент більше 0. take_integer – повинна бути рекурсивною функцією.

first_n_integers :: Integer -> [Integer]
first_n_integers (a) = take_integer (a, [1 ..])
	where 
	take_integer :: (Integer, [Integer]) -> [Integer]
	take_integer (n, list)
		| (n < 0) = error "(n < 0)" 
		| (n == 0) = [] 
		| (list == []) = [] 
		| otherwise = (head (list) : take_integer ((n-1), (tail list)))


-- 6. Визначити функцію double_factorial, котра отримує  Integer n   і вираховує добуток всіх факторіалів від 0 і до n включно. Використайте let або where вираз, щоб визначити функцію factorial, котра буде використовуватися функцією. Обидві функції factorial і double_factorial повинні бути рекурсивними.


double_factorial :: Integer -> Integer
double_factorial (n) 
	|n==1 = 1
	|otherwise = factorial(n) * double_factorial (n-1)
		where
		factorial :: Integer -> Integer
		factorial (n) 
			|n==1 = 1
			|otherwise = n * factorial (n-1)


-- 7. Визначіть нескінченний список факторіалів (назовіть його factorials), використовуючи функцію zipWith (котру ви можете знайти  в документації Prelude). Перший факторіал повинен бути факторіал 0, тобто 1. Це потрібно зробити в один рядок (плюс ще один рядок для визначення типу). Цей список факторіалів повинен мати тип [Integer]. Підказка: Ви можете використати нескінченний список факторіалів в вашому власному визначенні.


factorials :: [Integer]
factorials = 1:zipWith  ( * ) factorials [1..]


					

		 

	