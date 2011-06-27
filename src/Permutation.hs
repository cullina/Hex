module Permutation where

import Data.Monoid

data PermIndex = One | Two | Three | Four | Five
                 deriving Show

data Perm = Perm PermIndex PermIndex PermIndex PermIndex PermIndex
            deriving Show

instance Monoid Perm where
    mempty = Perm One Two Three Four Five

    mappend p (Perm a b c d e) = 
        Perm (f a) (f b) (f c) (f d) (f e)
            where f = g p 
                  g (Perm a _ _ _ _) One   = a
                  g (Perm _ b _ _ _) Two   = b
                  g (Perm _ _ c _ _) Three = c
                  g (Perm _ _ _ d _) Four  = d
                  g (Perm _ _ _ _ e) Five  = e
 


vertex = Perm Five One Two Three Four

edge   = Perm One Three Two Five Four

--face   = vertex times edge

