
module Tape where
import Data.List
import Data.Tuple

type Tape a = ( [a] , [a] )

initT       :: a -> Tape a
initT d     = ([],[d])

rightT      :: a -> Tape a -> Tape a
rightT d t  = let (ys,xs) = t 
                    in case xs of
                        []      -> (d:ys,[d])
                        [x]     -> (x:ys,[d])
                        (x:xs)  -> (x:ys, xs)

leftT      :: a -> Tape a -> Tape a
leftT d t  = let (ys,xs) = t 
                    in case ys of
                        []      -> ([],d:xs)
                        (y:ys)  -> (ys,y:xs)

readT       :: Tape a -> a
readT t     = let (ys,xs) = t
                    in (head xs)

writeT      :: a -> Tape a -> Tape a
writeT w t  = let (ys,xs) = t
                    in (ys,w:(tail xs))



                    
-- t = initT 0
-- a = rightT 0 $ rightT 0 $ rightT 0 $ leftT 0 $ leftT 0 $ leftT 0 $ leftT 0 $ leftT 0 $t