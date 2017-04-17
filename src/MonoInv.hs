module MonoInv where

import MonoSearch

monoInv :: RealFrac a => (a -> a) -> (a -> a)
monoInv = search


