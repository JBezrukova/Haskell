data WeirdPeanoNumber = Zero | Succ (WeirdPeanoNumber ) | Pred (WeirdPeanoNumber )

instance Eq WeirdPeanoNumber where
    (==) Zero Zero = True
    (==) (Succ a) (Succ b) = a == b    
    (==) (Pred a) (Pred b) = a == b
    (==) this other = False

instance Ord WeirdPeanoNumber where
    (<=) Zero Zero = True
    (<=) Zero (Succ a) = if (a < (-1)) then False else True
    (<=) Zero (Pred a) = if (a < 1) then False else True
 
    (<=) (Pred a) (Pred b) = a <= b
    (<=) (Succ a) (Succ b) = a <= b
 
    (<=) (Pred a) b = toInteger (a - 1) <= toInteger b
    (<=) (Succ a) b = toInteger (a + 1) <= toInteger b
 
    (<) Zero Zero = False
    (<) Zero (Succ a) = if (a < 0) then False else True
    (<) Zero (Pred a) = if (a > 1) then True else False
 
    (<) (Pred a) (Pred b) =  a < b 
    (<) (Succ a) (Succ b) =  a < b 
 
    (<) (Pred a) b = toInteger (a - 1) < toInteger b
    (<) (Succ a) b = toInteger (a + 1) < toInteger b

    (>) a b = not ((<) a b)
    (>=) a b = not ((<=) a b)

instance Num WeirdPeanoNumber where
    
    negate Zero = Zero
    negate (Pred a) = Succ (negate a)
    negate (Succ a) = Pred (negate a)
    
    abs Zero = Zero
    abs arg @(Pred a) | arg < Zero = Succ (abs a)
                      | otherwise = arg
    abs arg @(Succ a) | arg < Zero = Pred (abs a)
                      | otherwise = arg
   

    signum Zero = Zero
    signum arg @(Succ a) | arg > Zero = Succ Zero
                         | arg < Zero = Pred Zero
                         | otherwise = Zero
    signum arg @(Pred a) | arg > Zero =  Succ Zero
                         | arg < Zero = Pred Zero
                         | otherwise = Zero

    fromInteger val | (val == 0) = Zero
                    | (val > 0) = Succ $ fromInteger (val - 1)
                    | (val < 0) = Pred $ fromInteger (val + 1)

    (+) a Zero = a
    (+) Zero a = a
    (+) (Pred a) (Pred b) = a + Pred (Pred b)
    (+) (Succ a) (Succ b) = a + Succ (Succ b)
    (+) (Pred a) (Succ b) = a + b
    (+) (Succ a) (Pred b) = a + b

    (*) Zero _ = Zero
    (*) _ Zero = Zero
    (*) (Succ Zero) a = a
    (*) a (Succ Zero) = a
    (*) (Succ a) (Succ b) = (Succ a) * b + (Succ a)
    (*) arg1 @(Pred a) arg2 @(Pred b) = negate arg1 * negate arg2
    (*) arg1 @(Succ a) arg2 @(Pred b) = negate (arg1 * negate arg2)
    (*) arg1 @(Pred a) arg2 @(Succ b) = negate (negate arg1 * arg2)

instance Enum WeirdPeanoNumber where
    toEnum a |a == 0 = Zero
             |a > 0 = Succ( toEnum (a - 1))
             |a < 0 = Pred( toEnum (a + 1))

    fromEnum Zero = 0
    fromEnum (Succ a) = fromEnum a + 1
    fromEnum (Pred a) = fromEnum a - 1

instance Show WeirdPeanoNumber where
    show a = show (toInteger a)

instance Real WeirdPeanoNumber where
    toRational Zero = toRational 0
    toRational (Succ a) = toRational (toRational a + 1)
    toRational (Pred a) = toRational (toRational a - 1)

instance Integral WeirdPeanoNumber where
    toInteger Zero = toInteger 0
    toInteger (Succ a) = toInteger (toInteger a + 1)
    toInteger (Pred a) = toInteger (toInteger a - 1)

    quotRem a Zero = error "Can not be devided by zero"
    quotRem a b | (a == b) = (Succ (Zero), Zero)
                | abs a < abs b = (Zero, a)
                | a > 0 && b > 0 = quotrem' a b
                | otherwise = quotrem' (abs a) (abs b)
   
                where quotrem' a b | (a == b) = (Succ(Zero), Zero)
                                   | (a < b)  = (Zero, a)
                                   | (a > b)  = (Succ(fst (quotrem' (a-b) b)),snd (quotrem' (a-b) b))

    