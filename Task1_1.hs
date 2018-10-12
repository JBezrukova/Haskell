module Task1_1 where

import Todo(todo)

data Term =   IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | Add{ lhv :: Term, rhv :: Term } -- бинарная операция
            | Sub{ lhv :: Term, rhv :: Term } -- бинарная операция
            | Mult{ lhv :: Term, rhv :: Term } -- бинарная операция
            deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) (IntConstant l) (IntConstant r) = IntConstant (l + r)
(|+|) l r = Add l r
(|-|) :: Term -> Term -> Term
(|-|) (IntConstant l) (IntConstant r) = IntConstant (l - r)
(|-|) l r = Sub l r
(|*|) :: Term -> Term -> Term
(|*|) (IntConstant l) (IntConstant r) = IntConstant (l * r)
(|*|) l r = Mult l r
infixl 6 |+|, |-|
infixl 7 |*|
-- Заменить переменную `var` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar var replacement Variable {varName = varName'} | varName' == var = replacement
replaceVar var replacement Add{lhv = l, rhv = r}  = Add  (replaceVar var replacement l) (replaceVar var replacement r)
replaceVar var replacement Sub{lhv = l, rhv = r}  = Sub  (replaceVar var replacement l) (replaceVar var replacement r)
replaceVar var replacement Mult{lhv = l, rhv = r} = Mult (replaceVar var replacement l) (replaceVar var replacement r)
replaceVar _ _ a = a
-- Посчитать значение выражения `Term`
-- если оно состоит только из констант (и упрощает его по мере возможности, если нет).
evaluate :: Term -> Term
evaluate Add { lhv = IntConstant 0, rhv = r}             = evaluate r
evaluate Add { lhv = l,             rhv = IntConstant 0} = evaluate l
evaluate Add { lhv = l,             rhv = r}             = (|+|) (evaluate l) (evaluate r)

evaluate Sub { lhv = l,             rhv = IntConstant 0} = evaluate l
evaluate Sub { lhv = l,             rhv = r}             = (|-|) (evaluate l) (evaluate r)

evaluate Mult { lhv = IntConstant 1, rhv = r}             = evaluate r
evaluate Mult { lhv = l,             rhv = IntConstant 1} = evaluate l
evaluate Mult { lhv = l,             rhv = r}             = (|*|) (evaluate l) (evaluate r)
evaluate a = a