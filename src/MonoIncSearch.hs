module MonoIncSearch where

import PreludeCustom
import Debug.Trace
import Range


nextRange :: (Fractional t,Ord t) => (t -> t) -> t -> Range t -> Range t
nextRange f y r@(Range a b) = case rangeCase y (mapRange f r) of
    XAB -> range  (a - 2*(b-a)) $ a
    AXB -> range  a             $ a + (b-a)/2
    ABX -> range  b             $ b + 2*(b-a)

getX :: Range a -> a
getX = b

found :: (Eq a) => a -> (a -> a) -> Range a -> Bool
found y f r = (f . getX) r  == y

search :: (Fractional a, Ord a) => Int -> Range a -> (a -> a) -> (a -> a)
search 0 r _ _ = getX r
search i r f y | found y f r = getX r
               | otherwise   = search (i-1) (nextRange f y r) f y

-- search :: (Fractional a,Eq a, Ord a,Show a) => Int -> Range a -> (a -> a) -> (a -> a)
-- search 0 r _ _ = getX r
-- search i r f y | found y f r = getX r
--                | otherwise   = traceShow (show i <> ": " <> show r) $ search (i-1) (nextRange f y r) f y

