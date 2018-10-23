module Task2_1 where

import Todo(todo)
import Prelude hiding (lookup) 

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v =  EmptyTreeMap 
               | Leaf (Integer, v)
               | Node (Integer, v) (TreeMap v) (TreeMap v)
                deriving (Show, Eq, Read)

-- Пустое дерево
emptyTree :: TreeMap 
emptyTree = EmptyTreeMap

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap -> Integer -> Bool
contains EmptyTreeMap _ = False
contains (Leaf (a, b)) k | a == k = True
                         | otherwise = False
contains (Node (a, b) l r) k | k == a = True
                             | k >  a = contains r k
                             | k <  a = contains l k

-- Значение для заданного ключа
lookup :: Integer -> TreeMap -> v
lookup k EmptyTreeMap = error "The key does not belong to a key set"
lookup k (Leaf (a, b)) | k == a = b
                       | otherwise = error "The key does not belong to a key set"
lookup k (Node (a, b) l r) | k == a = b
                           | k >  a = lookup k r
                           | k <  a = lookup k l

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap -> TreeMap
insert (k, v) EmptyTreeMap = Leaf (k, v)
insert (k, v) (Leaf (a, b)) | k == a = Leaf (k, v) 
                            | k >  a = Node (a, b) EmptyTreeMap (Leaf (k, v))
                            | k <  a = Node (a, b) (Leaf (k, v)) EmptyTreeMap
insert (k, v) (Node (a, b) l r) | k == a = Node (k, v) l                 r
                                | k >  a = Node (a, b) l                 (insert (k, v) r)
                                | k <  a = Node (a, b) (insert (k, v) l) r

-- Удаление элемента по ключу
remove :: Integer -> TreeMap -> TreeMap
remove i EmptyTreeMap = error "TreeMap is empty"
remove i leaf@(Leaf (k, v)) | i == k = EmptyTreeMap
                            | otherwise = leaf
remove i (Node (a,b) l r) | i == a = replace l r
                          | i >  a = (Node (a, b) l (remove i r))
                          | i <  a = (Node (a, b) (remove i l) r)
replace EmptyTreeMap x = x
replace (Leaf (a, b)) x = (Node (a, b) EmptyTreeMap x)
replace (Node (a, b) l r) x = (Node (a, b) l replace (r x))
-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap -> (Integer, v)
nearestLE i EmptyTreeMap = todo

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap
treeFromList [] = EmptyTreeMap
treeFromList (x:xs) = insert x (treeFromList xs)
 
-- Построение списка пар из дерева
listFromTree :: TreeMap -> [(Integer, v)]
listFromTree EmptyTreeMap = []
listFromTree (Leaf (k, v)) = [(k, v)]
listFromTree (Node (k, v) l r) = listFromTree l ++ [(k, v)] ++ listFromTree r

-- Поиск k-той порядковой статистики дерева 
kMean :: Integer -> TreeMap -> (Integer, v)
kMean i t = listFromTree t !! fromIntegral(i)