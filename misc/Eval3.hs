module Eval3 (eval, example, runStateErrorCost, initState) where

import AST
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

-- Estados
type Env = [(Variable,Int)]

-- Estado nulo
initState :: Env
initState = []

-- Mónada estado, con manejo de errores y costo
newtype StateErrorCost a = StateErrorCost { runStateErrorCost :: Env -> Maybe (a,Int,Env) }


instance Monad StateErrorCost where
    return x = StateErrorCost (\s -> Just (x,0,s))
    m >>= f  = StateErrorCost (\s -> case runStateErrorCost m s of
                                          Nothing -> Nothing
                                          Just (v,c,s') -> case runStateErrorCost (f v) s' of
                                                              Nothing -> Nothing
                                                              Just (v',c',s'') -> Just (v',c'+c,s'') )
-- Para calmar al GHC
instance Functor StateErrorCost where
    fmap = liftM

instance Applicative StateErrorCost where
    pure   = return
    (<*>)  = ap

-- Clase para representar mónadas con estado de variables
class Monad m => MonadState m where
    -- Busca el valor de una variable
    lookfor :: Variable -> m Int
    -- Cambia el valor de una variable
    update :: Variable -> Int -> m ()

instance MonadState StateErrorCost where
    lookfor v = StateErrorCost (\s -> case lookfor' v s of
                                  Nothing -> Nothing
                                  Just a -> Just (a,0,s))
                where lookfor' v []                   = Nothing
                      lookfor' v ((u, j):ss) | v == u = Just j
                                             | v /= u = lookfor' v ss

    update v i = StateErrorCost (\s -> Just ((),0, update' v i s))
                 where update' v i [] = [(v, i)]
                       update' v i ((u, _):ss) | v == u = (v, i):ss
                       update' v i ((u, j):ss) | v /= u = (u, j):update' v i ss


-- Clase para representar mónadas que lanzan errores
class Monad m => MonadError m where
    -- Lanza un error
    throw :: m a

instance MonadError StateErrorCost where
    throw = StateErrorCost (\s -> Nothing)

-- Clase para representar mónadas que llevan el costo
class Monad m => MonadCost m where
    -- Agrega 1 al costo
    tick :: m ()

instance MonadCost StateErrorCost where
    tick = StateErrorCost (\s -> Just ((),1,s))

-- Evalua un programa en el estado nulo
eval :: Comm -> Maybe (Env,Int)
eval p = case (runStateErrorCost (evalComm p) initState) of
              Nothing -> Nothing
              Just (v,c,env) -> Just (env,c)

-- Evalua un comando en un estado dado
evalComm :: (MonadState m, MonadError m,MonadCost m) => Comm -> m ()
evalComm Skip = return ()
evalComm (Let var intExp) = do n <- evalIntExp intExp
                               update var n
evalComm (Seq comm1 comm2) = do evalComm comm1
                                evalComm comm2
evalComm (IfThenElse boolExp comm1 comm2) = do b <- evalBoolExp boolExp
                                               if b then evalComm comm1
                                                    else evalComm comm2
evalComm (While boolExp comm) = do b <- evalBoolExp boolExp
                                   if b then do evalComm comm
                                                evalComm (While boolExp comm)
                                        else evalComm Skip

-- Evalua una expresion entera en un estado dado
evalIntExp :: (MonadState m, MonadError m,MonadCost m) => IntExp -> m Int
evalIntExp (Const n) = return n
evalIntExp (Var var) = lookfor var
evalIntExp (UMinus ie) = do n <- evalIntExp ie
                            return (n*(-1))
evalIntExp (Plus ie1 ie2) = do n <- evalIntExp ie1
                               m <- evalIntExp ie2
                               tick
                               return (n+m)
evalIntExp (Minus ie1 ie2) = do n <- evalIntExp ie1
                                m <- evalIntExp ie2
                                tick
                                return (n-m)
evalIntExp (Times ie1 ie2) = do n <- evalIntExp ie1
                                m <- evalIntExp ie2
                                tick
                                tick
                                return (n*m)
evalIntExp (Div ie1 ie2) = do n <- evalIntExp ie1
                              m <- evalIntExp ie2
                              tick
                              tick
                              if m == 0 then throw
                                        else return (div n m)
evalIntExp (Ass var ie1) = do n <- evalIntExp ie1
                              update var n
                              lookfor var
evalIntExp (SeqIE ie1 ie2) = do evalIntExp ie1
                                evalIntExp ie2

-- Evalua una expresion booleana en un estado dado
evalBoolExp :: (MonadState m, MonadError m,MonadCost m) => BoolExp -> m Bool
evalBoolExp BTrue = return True
evalBoolExp BFalse = return False
evalBoolExp (Eq ie1 ie2) = do n <- evalIntExp ie1
                              m <- evalIntExp ie2
                              tick
                              return (n == m)
evalBoolExp (NEq ie1 ie2) = do n <- evalIntExp ie1
                               m <- evalIntExp ie2
                               tick
                               return (n /= m)
evalBoolExp (Lt ie1 ie2) = do n <- evalIntExp ie1
                              m <- evalIntExp ie2
                              tick
                              return (n < m)
evalBoolExp (Gt ie1 ie2) = do n <- evalIntExp ie1
                              m <- evalIntExp ie2
                              tick
                              return (n > m)
evalBoolExp (And be1 be2) = do b1 <- evalBoolExp be1
                               b2 <- evalBoolExp be2
                               tick
                               return (b1 && b2)
evalBoolExp (Or be1 be2) = do b1 <- evalBoolExp be1
                              b2 <- evalBoolExp be2
                              tick
                              return (b1 || b2)
evalBoolExp (Not be) = do b <- evalBoolExp be
                          tick
                          return (not b)

-- Example function using the StateErrorCost monad
example :: StateErrorCost ()
example = do
    -- Initialize the state with some variables
    update "x" 10
    update "y" 20

    -- Look up a variable
    x <- lookfor "x"
    y <- lookfor "y"

    -- Print the values (for demonstration purposes)
    StateErrorCost (\s -> Just (print (x, y), 0, s))

    -- Update a variable
    update "x" 30

    -- Increment the cost
    tick


-- Run the example function and print the result
main :: IO ()
main = do
    let result = runStateErrorCost example initState
    case result of
        Nothing -> putStrLn "An error occurred."
        Just ((), cost, finalState) -> do
            putStrLn $ "Final state: " ++ show finalState
            putStrLn $ "Total cost: " ++ show cost