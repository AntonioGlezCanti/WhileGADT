module NaturalSemantics where

import          WhileGADT


-- representation of configurations for While

data Config = Inter Stm State  -- <S, s>
            | Final State      -- s

-- representation of the transition relation <S, s> -> s'

nsStm :: Config -> Config

-- x := a

nsStm (Inter (Ass x a) s)      = Final (update s (x :=>: (eVal a s)))

-- skip

nsStm (Inter Skip s)           = Final s
-- s1; s2

nsStm (Inter (Comp ss1 ss2) s) = nsStm (Inter ss2 s1) 
  where
    Final s1 = nsStm (Inter ss1 s) 
-- if b then s1 else s2

-- B[b]s = tt
nsStm (Inter (If b ss1 ss2) s) 
  | eVal b s = nsStm (Inter ss1 s)
-- B[b]s = ff
nsStm (Inter (If b ss1 ss2) s) 
  | not (eVal b s) = nsStm (Inter ss2 s)
-- while b do s

-- B[b]s = ff
nsStm (Inter (While b ss) s) 
  | not (eVal b s) = Final s
-- B[b]s = tt
nsStm (Inter (While b ss) s) 
  | eVal b s = nsStm (Inter (Comp ss (While b ss)) s)

-- semantic function for natural semantics
sNs :: Stm -> State -> State
sNs ss s = s'
  where Final s' = nsStm (Inter ss s)

update :: State -> Update -> State
update s (y :=>: n) x = if x == y then n else (s x)

updates :: State ->  [Update] -> State
updates s [x] = update s x
updates s (x:xs) = updates (update s x) xs 

showState :: State -> [Var] -> [String]
showState s = map (\x -> x ++ " -> " ++ show(s x)) 

showFinalState :: Stm -> State -> [String]
showFinalState st s = showState (sNs st s) (fvStm st)

factorial :: Stm
factorial = Comp (Ass "y" (N 1))
                 (While (Neg (Eq (V "x") (N 1)))
                    (Comp (Ass "y" (Mult (V "y") (V "x")))
                          (Ass "x" (Sub (V "x") (N 1)))))

factorialInit :: State
factorialInit "x" = 3
factorialInit _   = 0