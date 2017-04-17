module MonoInv where

import MonoSearch

monoInv :: (Fractional a, Ord a) => (a -> a) -> (a -> a)
monoInv = search


