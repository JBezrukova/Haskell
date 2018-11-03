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
emptyTree :: TreeMap v
emptyTree = EmptyTreeMap

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains EmptyTreeMap _ = False
contains (Leaf (a, b)) k | a == k = True
                         | otherwise = False
contains (Node (a, b) l r) k | k == a = True
                             | k >  a = contains r k
                             | k <  a = contains l k

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> v
lookup k EmptyTreeMap = error "The key does not belong to a key set"
lookup k (Leaf (a, b)) | k == a = b
                       | otherwise = error "The key does not belong to a key set"
lookup k (Node (a, b) l r) | k == a = b
                           | k >  a = lookup k r
                           | k <  a = lookup k l

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (k, v) EmptyTreeMap = Leaf (k, v)
insert (k, v) (Leaf (a, b)) | k == a = Leaf (k, v) 
                            | k >  a = Node (a, b) EmptyTreeMap (Leaf (k, v))
                            | k <  a = Node (a, b) (Leaf (k, v)) EmptyTreeMap
insert (k, v) (Node (a, b) l r) | k == a = Node (k, v) l                 r
                                | k >  a = Node (a, b) l                 (insert (k, v) r)
                                | k <  a = Node (a, b) (insert (k, v) l) r

-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v
remove i EmptyTreeMap = error "TreeMap is empty"
remove i leaf@(Leaf (k, v)) | i == k = EmptyTreeMap
                            | otherwise = leaf
remove i (Node (a,b) l r) | i == a = replace l r
                          | i >  a = (Node (a, b) l (remove i r))
                          | i <  a = (Node (a, b) (remove i l) r)
replace :: TreeMap v -> TreeMap v -> TreeMap v
replace EmptyTreeMap x = x
replace (Leaf (a, b)) x = (Node (a, b) EmptyTreeMap x)
replace (Node (a, b) l r) x = (Node (a, b) l (replace r x))

-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE i EmptyTreeMap = error "The key does not belong to a key set"

nearestLE i (Leaf (a, b)) | i >= a = (a, b)
                          | otherwise = error "No nearest element founded"

nearestLE i (Node (a, b) l r) | i == a = (a, b)
                              | i < a && isLeaf l = if ((fst $ getPare l) <= i) then getPare l else error "No nearest element founded"
                              | i < a && isNode l = if (i > (fst $ getPare l)) then if isNode (getRight l) then nearestLE i (getRight l) 
                                                                                        else 
                                                                                             if isEmpty (getRight l) then (getPare l)
                                                                                                 else 
                                                                                                     if ((fst $ getPare (getRight l)) > i) then (getPare l)
                                                                                                         else nearestLE i (getRight l)
                                                                                   else nearestLE i l
                              | i > a && isEmpty r = (a, b) 
                              | i < a && isEmpty l = error "No nearest element founded"
                              | otherwise = if (getLastLeftKey r) <= i then nearestLE i r else (a, b)


isLeaf :: TreeMap v -> Bool
isLeaf (Leaf (a, b)) = True
isLeaf _ = False

isNode :: TreeMap v -> Bool
isNode (Node (a, b) l r) = True
isNode _ = False

isEmpty :: TreeMap v -> Bool
isEmpty EmptyTreeMap = True
isEmpty _ = False

getRight :: TreeMap v -> TreeMap v
getRight (Node (a, b) l r) = r

getPare :: TreeMap v -> (Integer, v)
getPare (Leaf (a, b)) = (a, b)
getPare (Node (a, b) l r) = (a, b)

getLastLeftKey :: TreeMap v -> Integer
getLastLeftKey (Leaf (a, b)) = a
getLastLeftKey (Node (a, b) EmptyTreeMap _) = a
getLastLeftKey (Node (a, b) l _) = getLastLeftKey l
                               
-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList [] = EmptyTreeMap
treeFromList (x:xs) = insert x (treeFromList xs)
 
-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree EmptyTreeMap = []
listFromTree (Leaf (k, v)) = [(k, v)]
listFromTree (Node (k, v) l r) = listFromTree l ++ [(k, v)] ++ listFromTree r

-- Поиск k-той порядковой статистики дерева 
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean i EmptyTreeMap = error "TreeMap is empty"
kMean i (Leaf (a, b)) | i == 0 = (a, b)
                      | otherwise = error "Index not exists"

kMean i (Node (a, b) EmptyTreeMap (Leaf (c, d))) | i == 0 = (a, b)
                                                 | i == 1 = (c, d)
                                                 | otherwise = error "Index not exists"
kMean i (Node (a, b) (Leaf (c, d)) EmptyTreeMap) | i == 0 = (a, b)
                                                 | i == 1 = (c, d)
                                                 | otherwise = error "Index not exists"
kMean i (Node (a, b) (Leaf (c, d)) (Leaf (e, f))) | i == 0 = (c, d)
                                                  | i == 1 = (a, b)
                                                  | i == 2 = (e, f)
                                                  | otherwise = error "Index not exists"
kMean i (Node (a, b) l r) | size l >= (i - 1) = kMean i l
                          | size l ==  i = (a, b)
                          | otherwise = kMean (i - (size l + 1)) r

size :: TreeMap v -> Integer
size EmptyTreeMap = (-1)
size (Leaf (a, b)) = 1
size (Node (a, b) l r) = size l + size r + 1