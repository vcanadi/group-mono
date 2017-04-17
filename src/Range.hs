module Range where

-- Range a b: <a,b]
data Range t   = Range { a::t, b::t }   deriving (Eq,Ord,Show)
data XPosition = XAB | AXB | ABX        deriving (Eq,Ord,Show)

range :: (Ord t) => t -> t -> Range t
range a b = Range (min a b) (max a b)

mapRange :: (Ord s) => (t -> s) -> Range t -> Range s
mapRange f (Range a b) = range (f a) (f b)


-- XAB: ---X---A----B-------
-- AXB: -------A--X-B-------
-- ABX: -------A----B-----X-
rangeCase :: (Ord t) => t -> Range t -> XPosition
rangeCase x (Range a b) | x <= a = XAB
                        | x <= b = AXB
                        | True   = ABX


range01 :: (Num t,Ord t) => Range t
range01 = range 0 1


