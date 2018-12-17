data WeirdPeanoNumber = Zero | Succ (WeirdPeanoNumber ) | Pred (WeirdPeanoNumber )

instance Eq WeirdPeanoNumber where
    (==) Zero Zero = True
    (==) (Succ a) (Succ b) = a == b    
    (==) (Pred a) (Pred b) = a == b
    (==) _ _ = False


reduce :: WeirdPeanoNumber -> WeirdPeanoNumber
reduce Zero = Zero
reduce (Succ (Pred a)) = reduce a
reduce (Pred (Succ a)) = reduce a
reduce (Succ a) = case reduce a of (Pred b) -> b
                                   _ -> Succ (reduce a)
reduce (Pred a) = case reduce a of (Succ b) -> b
                                   _ -> Pred (reduce a)

quotRem' a Zero = error "Can not be devided by zero"
quotRem' a b | (a == b) = (Succ (Zero), Zero)
             | abs a < abs b = (Zero, a)
             | a > 0 && b > 0 = quotrem' a b
             | otherwise = quotrem' (abs a) (abs b)
   
            where quotrem' a b | (a == b) = (Succ(Zero), Zero)
                               | (a < b)  = (Zero, a)
                               | (a > b)  = (Succ(fst (quotrem' (a-b) b)),snd (quotrem' (a-b) b))


instance Ord WeirdPeanoNumber where
     (<=) a b = lessOrEqual (reduce a) (reduce b) where
      lessOrEqual (Succ a) (Succ b) = lessOrEqual a b
      lessOrEqual (Pred a) (Pred b) = lessOrEqual a b
      lessOrEqual (Pred _) Zero = True
      lessOrEqual Zero (Succ _) = True
      lessOrEqual a b = a == b

instance Num WeirdPeanoNumber where
    
    negate Zero = Zero
    negate (Pred a) = Succ (negate a)
    negate (Succ a) = Pred (negate a)
    
    abs a | signum a == 0 = Zero
          | signum a == 1 = a
          | signum a == (-1) = negate a
   

    signum a | a == Zero = 0
             | a < Zero = (-1)
             | otherwise = 1

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
    toRational a = toRational (toInteger a)

instance Integral WeirdPeanoNumber where
    toInteger Zero = toInteger 0
    toInteger (Succ a) = toInteger a + 1
    toInteger (Pred a) = toInteger a - 1

    quotRem a b = quotRem' (reduce a) (reduce b)

    