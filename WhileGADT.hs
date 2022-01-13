{-# LANGUAGE GADTs, StandaloneDeriving #-}

module WhileGADT where

type Var = String
type Z = Integer
type  State = Var -> Z

data Exp a where 
    N :: Z -> Exp Z
    V :: Var -> Exp Var
    TRUE :: Exp Bool
    FALSE :: Exp Bool
    Eq :: Exp Z -> Exp Z -> Exp Bool
    Le :: Exp Z -> Exp Z -> Exp Bool
    Neg :: Exp Bool -> Exp Bool
    And :: Exp Bool -> Exp Bool -> Exp Bool
    Add :: Exp Z -> Exp Z -> Exp Z
    Mult :: Exp Z -> Exp Z -> Exp Z
    Sub :: Exp Z -> Exp Z -> Exp Z
deriving instance Show (Exp a)

aVal :: Exp a -> State -> a
aVal (N n) _        =  n
--aVal (V x) s        =  s x
aVal (Add a1 a2) s  =  aVal a1 s + aVal a2 s
aVal (Mult a1 a2) s =  aVal a1 s * aVal a2 s
aVal (Sub a1 a2) s  =  aVal a1 s - aVal a2 s
aVal TRUE _        =  True
aVal FALSE _       =  False
aVal (Eq a1 a2) s  =  aVal a1 s == aVal a2 s
aVal (Le a1 a2) s  =  aVal a1 s <= aVal a2 s
aVal (Neg b) s     =  not(aVal b s)
aVal (And b1 b2) s =  aVal b1 s && aVal b2 s

e :: Exp Z
e = Add (N 3) (N 4)

e1 :: Exp Bool
e1 = Le (Add (N 3) (N 4)) (N 5)


s :: State
s x = 5
s _ = 0
