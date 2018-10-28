module Task2_2 where

import Todo(todo)

import Prelude hiding (foldr, foldl, unfoldr, map, concatMap, 
    filter, maxBy, minBy, reverse, sum, product, elem)
import Data.List (splitAt)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f a [] = a
foldl f a (x : xs) = foldl f (f a x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f a [] = a
foldr f a (x : xs) = x `f` foldr f a xs
 
unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f a = case f a of         
    Just (x, y)-> x : unfoldr f y
    Nothing -> []

-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum xs = foldl (+) 0 xs

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse xs = foldl f [] xs where f t h = h:t

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map f = foldr (\x y -> (f x) : y) []

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product xs = foldr (*) 1 xs

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (\ x y -> case x of Just a -> (a : y)
                                      Nothing -> y    ) []

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal lst = snd (foldr (\xs (x, y) -> (x - 1, xs !! x : y)) (k, []) lst)
               where k = length lst - 1

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot p = foldr (\x y -> if p x then y else (x : y)) []

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem a = foldr (\x y -> if (x == a) then True else y) False

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = unfoldr f from where
         f = (\x -> if (x) >= (to) then Nothing else Just (x, x + step))

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append a b  = foldr (:) b a 

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups lst n = unfoldr (\xs -> if null xs then Nothing else (Just (splitAt k xs))) lst 
               where k = fromIntegral n