
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

-------------------------
------State Monad--------

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

-- Clase para representar mónadas con estado de variables
class Monad m => MonadState m where
    right ::               m ()
    left  ::               m ()
    write ::  Symbols ->   m ()
    read  ::               m Symbols

-- Accesos la cinta solo con las maquinas elementales
instance MonadState State where
    right   = State(\s -> (()       ,rightT    s)  )
    left    = State(\s -> (()       ,leftT     s)  )
    write w = State(\s -> (()       ,writeT  w s)  )
    read    = State(\s -> ((readT s),  s        )  )

-- -- Evalua un programa en el estado nulo
-- eval :: Comm -> Env
-- eval p = snd (runState (evalComm p) initState)
------------------------------
----- ELEMENTAL MACHINES -----

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

rightUntil      :: MonadState m => Symbols -> m()
rightUntil s    =   do  i <- Tape.read
                        if i == s 
                            then return ()  
                            else do right
                                    rightUntil s
                            -- else right >>= (rightUntil s)

leftUntil      :: MonadState m => Symbols -> m()
leftUntil s    =    do  i <- Tape.read
                        if i == s 
                            then return ()  
                            else do left
                                    leftUntil s
                            -- else right >>= (rightUntil s)
-- leftUntil       :: MonadState m => Symbols -> m()
--Mas que Expresion, puede ser que Secuencia del lectura se ajuste mejor a lo que hace
rightUntilExpr      :: MonadState m => [Symbols] -> m()
rightUntilExpr []       = return ()
rightUntilExpr xs   = do    i <- Tape.read
                            if i == head xs 
                             then do    b <- checkExprRight xs
                                        if b
                                            then return ()
                                            else do right
                                                    rightUntilExpr xs 
                             else do    right
                                        rightUntilExpr xs

checkExprRight :: MonadState m => [Symbols] -> m Bool
checkExprRight []       = do    left
                                return True
checkExprRight (x:xs)   = do    i <- Tape.read
                                if( i == x )
                                    then do right
                                            b <- checkExprRight xs
                                            if b
                                                then return b 
                                                else do left 
                                                        return b
                                    else return False

leftUntilExpr      :: MonadState m => [Symbols] -> m()
leftUntilExpr []   = return ()
leftUntilExpr xs   = do    i <- Tape.read
                           if i == head xs 
                             then do    b <- checkExprLeft xs
                                        if b
                                            then return ()
                                            else do left
                                                    leftUntilExpr xs 
                             else do    left
                                        leftUntilExpr xs

checkExprLeft :: MonadState m => [Symbols] -> m Bool
checkExprLeft []       = do    right
                               return True
checkExprLeft (x:xs)   = do    i <- Tape.read
                               if( i == x )
                                    then do left
                                            b <- checkExprLeft xs
                                            if b
                                                then return b 
                                                else do right 
                                                        return b
                                    else return False


-- ([],[1,2,3,4,5])
del1al5 = writeT 1 $ leftT $ writeT 2 $ leftT $ writeT 3 $ leftT $ writeT 4 $ leftT $ writeT 5 $ initState


-- eval :: MonadState m => m() ->  Env
eval f initS =  runState ( f ) initS
                    
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

