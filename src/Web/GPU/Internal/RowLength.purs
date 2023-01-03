module Web.GPU.Internal.RowLength where

import Prim.Int (class Add)
import Prim.RowList (Cons, Nil, RowList)

class RowLength :: forall k1. RowList k1 -> Int -> Constraint
class RowLength rl i | rl -> i

instance RowLength Nil 0
instance (RowLength rest x, Add x 1 y) => RowLength (Cons key val rest) y
