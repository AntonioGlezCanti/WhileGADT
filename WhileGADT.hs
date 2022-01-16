{-# LANGUAGE GADTs, StandaloneDeriving #-}

module WhileGADT where

import  Data.List

type Var = String
type Z = Integer
type  State = Var -> Z


data Subst = Var :->: Exp Z

data Exp a where 
    N :: Z -> Exp Z
    V :: Var -> Exp Z
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

eVal :: Exp a -> State -> a
eVal (N n) _        =  n
eVal (V x) s        =  s x
eVal TRUE _        =  True
eVal FALSE _       =  False
eVal (Add a1 a2) s  =  eVal a1 s + eVal a2 s
eVal (Mult a1 a2) s =  eVal a1 s * eVal a2 s
eVal (Sub a1 a2) s  =  eVal a1 s - eVal a2 s
eVal (Eq a1 a2) s  =  eVal a1 s == eVal a2 s
eVal (Le a1 a2) s  =  eVal a1 s <= eVal a2 s
eVal (Neg b) s     =  not(eVal b s)
eVal (And b1 b2) s =  eVal b1 s && eVal b2 s


fvExp :: (Exp a) -> [Var]
fvExp e = nub (fv e)
    where
        fv :: Exp a -> [Var]
        fv (N _) = []
        fv (V x) = [x]
        fv (TRUE) = []
        fv (FALSE) = []
        fv (Add a1 a2) = fvExp a1 ++ fvExp a2
        fv (Mult a1 a2) = fvExp a1 ++ fvExp a2
        fv (Sub a1 a2) = fvExp a1 ++ fvExp a2
        fv (Eq a1 a2) = fvExp a1 ++ fvExp a2
        fv (Le a1 a2) = fvExp a1 ++ fvExp a2
        fv (Neg b) = fvExp b
        fv (And b1 b2) = fvExp b1 ++ fvExp b2


substExp :: Exp a -> Subst -> Exp a
substExp a s@(y :->: a1) = sub a
    where
        sub (N n) = N n
        sub (V x)   | x == y    = a1
                    | otherwise = (V x)
        sub (TRUE) = TRUE
        sub (FALSE) = FALSE
        sub (Add a1 a2) = Add (sub a1) (sub a2)
        sub (Mult a1 a2) = Mult (sub a1) (sub a2)
        sub (Sub a1 a2) = Sub (sub a1) (sub a2)                          
        sub (Eq a1 a2) = Eq (substExp a1 s) (substExp a2 s)
        sub (Le a1 a2) = Le (substExp a1 s) (substExp a2 s)
        sub (Neg b) = Neg (sub b)
        sub (And b1 b2) = And (sub b1) (sub b2)  



e :: Exp Z
e = Add (V "x") (N 4)

e1 :: Exp Bool
e1 = Le (Add (N 3) (N 4)) (N 5)

e2 = And (Eq (Add (V "y") (V "x")) (V "z")) (Neg (Le (V "x") (V "h")))

s :: State
s x = 5
s _ = 0
