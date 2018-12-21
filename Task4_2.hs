module Task4_2 where
data FourOf a = FourOf a a a a deriving(Show,Eq)

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FourOf`
-- таким образом, что 
-- do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } === FourOf 5 8 10 12

instance Functor (FourOf) where
    fmap f (FourOf a b c d) = FourOf (f a) (f b) (f c) (f d)

instance Applicative (FourOf) where
    pure a = FourOf a a a a
    (<*>) (FourOf a b c d) (FourOf e f g h) = FourOf (a e) (b f) (c g) (d h)


returnFirstElement (FourOf a _ _ _)  = a
returnSecondElement (FourOf _ a _ _) = a
returnThirdElement (FourOf _ _ a _)  = a
returnFourthElement (FourOf _ _ _ a) = a


instance Monad (FourOf) where
	return a = FourOf a a a a
	(FourOf a b c d) >>= f = FourOf (returnFirstElement (f a)) (returnSecondElement (f b)) (returnThirdElement (f c)) (returnFourthElement (f d))

x = do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y }
test = x ==  FourOf 5 8 10 12
     
