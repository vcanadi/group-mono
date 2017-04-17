module GroupMono where

import PreludeCustom
import Data.Monoid.Endo hiding (endo)
import Data.Group

import MonoInv

newtype Mono a = Mono { endo :: Endo a }

mono :: (a -> a) -> Mono a
mono = Mono . Endo

appMono :: Mono a -> (a -> a)
appMono = appEndo . endo

instance Monoid (Mono a) where
    mempty  = mempty endo
    mappend = (Mono .) . mappend `on` endo

instance RealFrac a => Group (Mono a) where
    invert  =  mono . monoInv . appMono
