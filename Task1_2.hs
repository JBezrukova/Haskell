module Task1_2 where

import Todo(todo)

-- синус числа (формула Тейлора)
sin' :: Double -> Double
sin' x = todo
-- косинус числа (формула Тейлора)
cos' :: Double -> Double
cos' x = todo
-- наибольший общий делитель двух чисел
gcd' :: Integer -> Integer -> Integer
gcd' x y | x == 0 = abs y
         | y == 0 = abs x
         | (abs x < abs y) = gcd' y x
         | otherwise = gcd' y (x `mod` y)

-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to = (ceiling $ sqrt $ fromIntegral from) <= (floor $ sqrt $ fromIntegral to)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year | month `mod` 2 /= 0 || month == 8 && day >= 1 && day <= 31 = True
                             | month `mod` 2 == 0 && month /= 2 && day >= 1 && day <= 30 = True
                             | year `mod` 4 == 0 && month == 2 && day >= 1 && day <= 29 = True
                             | year `mod` 4 /= 0 && month == 2 && day >= 1 && day <= 28 = True
                             | otherwise = False


-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow x y | y == 0 = 1
		    | even y = result
		    | odd y  = x * result
      	  where result = pow (x * x) (y `div` 2)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x | x <= 1    = False
		      | x == 2    = True
          | otherwise = check x 2

check :: Integer -> Integer -> Bool
check a i | i == round(sqrt $ fromIntegral a) && a `mod` i == 0 = False
          | i == round(sqrt $ fromIntegral a) && a `mod` i /= 0 = True
		      | a `mod` i == 0                                      = False
		      | otherwise                                           = check a (i + 1)

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = todo

-- треугольник задан своими координатами.
-- функция должна вернуть 
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Point2D -> Point2D -> Point2D -> Integer
triangleKind a b c = todo