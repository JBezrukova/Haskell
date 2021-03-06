﻿module Task3_2 where

import Todo(todo)

data ReverseList a = RNil | RCons (ReverseList a) a
-- No instance for (Foldable ReverseList) 
-- Implementing Foldable for a data structure requires writing just one function: either foldMap or foldr
instance Foldable ReverseList where
  foldr function a RNil = a
  foldr function a (RCons b c) = function c (foldr function a b)

rlistToList :: ReverseList a -> [a]
rlistToList = foldl(\ a b -> b : a) [] 

listToRList :: [a] -> ReverseList a
listToRList [] = RNil
listToRList list = foldl (\ a b -> RCons a b) RNil list

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor

instance (Show a) => Show (ReverseList a) where
    show RNil = "[]"
    show (RCons RNil a) = show a
    show (RCons a b) = show a ++ "," ++ show b

instance (Eq a) => Eq (ReverseList a) where
    (==) RNil RNil = True
    (==) (RCons a b) (RCons a1 b1) = a == a1 && b == b1
    (==) _ _ = False

instance (Ord a) => Ord (ReverseList a) where
    (<=) a b = rlistToList a <= rlistToList b 

instance Semigroup (ReverseList a) where
    (<>) a RNil = a
    (<>) RNil a = a
    (<>) a (RCons b c) = RCons (mappend a b) c

instance Monoid (ReverseList a) where
    mempty = RNil
    mappend = (<>)

instance Functor ReverseList where
    fmap f RNil = RNil
    fmap f (RCons a b) = RCons (fmap f a) (f b)  