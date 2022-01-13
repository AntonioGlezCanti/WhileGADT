-- -------------------------------------------------------------------
--
-- While.hs
--
-- Abstract syntax of While
-- [Nielson and Nielson, Semantics with Applications]
--
-- -------------------------------------------------------------------

module While where

import           Aexp
import           Bexp

-- abstract syntax of While

data Stm = Ass Var Aexp
         | Skip
         | Comp Stm Stm
         | If Bexp Stm Stm
         | While Bexp Stm
         | Repeat Stm Bexp           -- | todo
         | For Var Aexp Aexp Stm     -- | todo
         | Abort                     -- | todo
         deriving Show
