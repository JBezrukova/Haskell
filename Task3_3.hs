
module Task3_3 where

newtype PSet a = PSet{ contains :: (a -> Bool) }
newtype PSet1 a = PSet1{ contains1 :: (a -> Bool) }
newtype PSet2 a = PSet2{ contains2 :: (a -> Bool) }
-- Реализуйте классы Monoid и Functor
-- Объясните в комментариях, почему они реализованы именно так

-- Основной закон моноида:
-- x `mappend` mzero === mzero `mappend` x === x

-- => могут быть реализованы : объединение, пересечение (при условии, что mempty -> True), симметричная разность
-- разница множеств не является симметричной операцией

-- объединение множеств (содержится ли элемент в одном из множеств)
-- *Task3_3> mappend (PSet (== "asd")) (PSet (== "sdf")) `contains` "dfg"                                                
-- False 
-- *Task3_3> mappend (PSet (== "asd")) (PSet (== "dfg")) `contains` "dfg"                                                 
-- True  

instance Semigroup (PSet a) where
    (<>) (PSet a) (PSet b) = PSet (\c -> (||) (a c) (b c))

instance Monoid (PSet a) where
    mempty = PSet (\x -> False)
    mappend = (<>)

-- пересечение множеств (содержится ли элемент в каждом из множеств)
-- *Task3_3> mappend (PSet1 (== "asd")) (PSet1 (== "dfg")) `contains1` "dfg"                                              
-- False    

instance Semigroup (PSet1 a) where
    (<>) (PSet1 a) (PSet1 b) = PSet1 (\c -> (&&) (a c) (b c))

instance Monoid (PSet1 a) where
    mempty = PSet1 (\x -> True)
    mappend = (<>)

instance Semigroup (PSet2 a) where
    (<>) (PSet2 a) (PSet2 b) = PSet2 (\c -> ((&&) (not $ a c) (b c) || (&&) (a c) (not $ b c)))

instance Monoid (PSet2 a) where
    mempty = PSet2 (\x -> False)
    mappend = (<>)

-- class Functor f where
-- fmap :: (a -> b) -> f a -> f b
-- требуется функция следующего вида: (PSet a -> PSet b). Не имея информации о типе a невозможно определить такую функцию

instance Functor PSet where
    fmap _ _ = PSet (\_ -> False)
