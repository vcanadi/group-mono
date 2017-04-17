module MonoSearch where

import qualified MonoIncSearch as MonoInc
import Range
import PreludeCustom

-- increasing or decreasing
isInc :: (Num a,Ord a) => (a -> a) -> Bool
isInc f = f (fromIntegral 0) < f (fromIntegral 1)

toInc, toDec :: (Num a,Ord a) => (a -> a) -> (a -> a)
toInc = bool <$> id <*> fmap negate <*> (not . isInc)
toDec = bool <$> id <*> fmap negate <*> isInc


d = 100
search :: RealFrac a => (a -> a) -> (a -> a)
search f  | isInc f = MonoInc.search d range01 f
          | True    = toDec $ MonoInc.search d range01 (toInc f)


