module Task4_1 where

-- Монада над функцией. В качестве входного значения `fun` может быть что угодно
-- Собственно, почему бы не `String`?

data FunMonad a = FunMonad { fun :: String -> a }

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FunMonad`

instance Functor (FunMonad) where
fmap a (FunMonad b) = FunMonad (a . b)

instance Applicative (FunMonad) where
pure = return
(<*>) = f
	where f a b = do
		c <- a
		d <- b
		return (c d)

instance Monad (FunMonad) where
return a = FunMonad (\_ -> a)
(FunMonad x) >>= f = FunMonad (\a -> (fun $ f $ (x a)) $ a)
