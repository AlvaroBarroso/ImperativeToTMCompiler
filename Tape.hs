
module Tape where
import Data.List
import Data.Tuple


import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)


--data Alphabet   = Zero     | One    deriving (Show)
--data Symbols    = Zero  | One | Empty  deriving (Show)
-- data Symbols    =  S Int | Empty  deriving (Show,Eq)




--- Symbols = Int
type Symbols    =  Int  
empty = 0

--- Symbols = Binary
-- data Symbols    = Zero  | One | Empty  deriving (Show)
-- empty = Empty   


type Tape a     = ( [a] , [a] )


initT       :: Tape Symbols
initT      = ([],[empty])

-- Estados
type Env = Tape Symbols

-- Estado nulo
initState :: Env
initState = initT

-- Mónada estado
newtype State a = State { runState :: Env -> (a, Env) }

instance Monad State where
    return x = State (\s -> (x, s))
    m >>= f = State (\s -> let (v, s') = runState m s in
                           runState (f v) s')

-- Para calmar al GHC
instance Functor State where
    fmap = liftM

instance Applicative State where
    pure   = return
    (<*>)  = ap



----- BASIC MACHINES -----



rightT      ::  Tape Symbols -> Tape Symbols
rightT t    = let (ys,xs) = t 
                      in case xs of
                      []      -> (empty:ys,[empty])
                      [x]     -> (x:ys,[empty])
                      (x:xs)  -> (x:ys, xs)


leftT       :: Tape Symbols -> Tape Symbols
leftT t     = let (ys,xs) = t 
                    in case ys of
                        []      -> ([empty],empty:xs)
                        (y:ys)  -> (ys,y:xs)


readT       :: Tape Symbols -> Symbols
readT t     = let (ys,xs) = t
                    in (head xs)


writeT      :: Symbols -> Tape Symbols -> Tape Symbols
writeT w t  = let (ys,xs) = t
                    in (ys,w:(tail xs))

----------------------------

----- Advance Machines -----

-- rightUntil:: Symbols -> Tape Symbols -> Tape Symbols






                    
-- t = initT 0
-- a = rightT 0 $ rightT 0 $ rightT 0 $ leftT 0 $ leftT 0 $ leftT 0 $ leftT 0 $ leftT 0 $t
-- a = rightT $ rightT $ rightT $ leftT $ leftT $ leftT $ leftT $ leftT $ initT
-- rightT $ leftT $ initT

-- leftT $ leftT $ leftT $ writeT One $ rightT $ writeT Zero $ rightT $ writeT Zero $ initT
-- leftT $ leftT $ leftT $ writeT 1 $ rightT $ writeT 2 $ rightT $ writeT 3 $ initT






-- OLD
-- initT       :: a -> Tape a
-- initT d     = ([],[d])

-- rightT      :: a -> Tape a -> Tape a
-- rightT d t  = let (ys,xs) = t 
--                     in case xs of
--                         []      -> (d:ys,[d])
--                         [x]     -> (x:ys,[d])
--                         (x:xs)  -> (x:ys, xs)

-- leftT      :: a -> Tape a -> Tape a
-- leftT d t  = let (ys,xs) = t 
--                     in case ys of
--                         []      -> ([],d:xs)
--                         (y:ys)  -> (ys,y:xs)

-- readT       :: Tape a -> a
-- readT t     = let (ys,xs) = t
--                     in (head xs)

-- writeT      :: a -> Tape a -> Tape a
-- writeT w t  = let (ys,xs) = t
--                     in (ys,w:(tail xs))

