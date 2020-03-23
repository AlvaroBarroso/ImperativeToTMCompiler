module Eval2 (eval) where

import AST
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

-- Estados
type Env = [(Variable,Int)]

-- Estado nulo
initState :: Env
initState = []

-- Mónada estado, con manejo de errores
newtype StateError a = StateError { runStateError :: Env -> Maybe (a, Env) }


instance Monad StateError where
    return x = StateError (\s -> Just (x, s))
    m >>= f  = StateError (\s -> case runStateError m s of
                                Nothing -> Nothing
                                Just (v,s') -> runStateError (f v) s')

-- Para calmar al GHC
instance Functor StateError where
    fmap = liftM

instance Applicative StateError where
    pure   = return
    (<*>)  = ap

-- Clase para representar mónadas con estado de variables
class Monad m => MonadState m where
    -- Busca el valor de una variable
    lookfor :: Variable -> m Int
    -- Cambia el valor de una variable
    update :: Variable -> Int -> m ()

instance MonadState StateError where
    lookfor v = StateError (\s -> case lookfor' v s of
                                  Nothing -> Nothing
                                  Just a -> Just (a, s))
                where lookfor' v []                   = Nothing
                      lookfor' v ((u, j):ss) | v == u = Just j
                                             | v /= u = lookfor' v ss

    update v i = StateError (\s -> Just ((), update' v i s))
                 where update' v i [] = [(v, i)]
                       update' v i ((u, _):ss) | v == u = (v, i):ss
                       update' v i ((u, j):ss) | v /= u = (u, j):(update' v i ss)


-- Clase para representar mónadas que lanzan errores
class Monad m => MonadError m where
    -- Lanza un error
    throw :: m a

instance MonadError StateError where
    throw = StateError (\s -> Nothing)

-- Evalua un programa en el estado nulo
eval :: Comm -> Maybe Env
eval p = case (runStateError (evalComm p) initState) of
              Nothing -> Nothing
              Just x -> Just (snd x)

-- Evalua un comando en un estado dado
evalComm :: (MonadState m, MonadError m) => Comm -> m ()
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
evalIntExp :: (MonadState m, MonadError m) => IntExp -> m Int
evalIntExp (Const n) = return n
evalIntExp (Var var) = lookfor var
evalIntExp (UMinus ie) = do n <- evalIntExp ie
                            return (n*(-1))
evalIntExp (Plus ie1 ie2) = do n <- evalIntExp ie1
                               m <- evalIntExp ie2
                               return (n+m)
evalIntExp (Minus ie1 ie2) = do n <- evalIntExp ie1
                                m <- evalIntExp ie2
                                return (n-m)
evalIntExp (Times ie1 ie2) = do n <- evalIntExp ie1
                                m <- evalIntExp ie2
                                return (n*m)
evalIntExp (Div ie1 ie2) = do n <- evalIntExp ie1
                              m <- evalIntExp ie2
                              if m == 0 then throw
                                        else return (div n m)
evalIntExp (Ass var ie1) = do n <- evalIntExp ie1
                              update var n
                              lookfor var
evalIntExp (SeqIE ie1 ie2) = do evalIntExp ie1
                                evalIntExp ie2

-- Evalua una expresion booleana en un estado dado
evalBoolExp :: (MonadState m, MonadError m) => BoolExp -> m Bool
evalBoolExp BTrue = return True
evalBoolExp BFalse = return False
evalBoolExp (Eq ie1 ie2) = do n <- evalIntExp ie1
                              m <- evalIntExp ie2
                              return (n == m)
evalBoolExp (NEq ie1 ie2) = do n <- evalIntExp ie1
                               m <- evalIntExp ie2
                               return (n /= m)
evalBoolExp (Lt ie1 ie2) = do n <- evalIntExp ie1
                              m <- evalIntExp ie2
                              return (n < m)
evalBoolExp (Gt ie1 ie2) = do n <- evalIntExp ie1
                              m <- evalIntExp ie2
                              return (n > m)
evalBoolExp (And be1 be2) = do b1 <- evalBoolExp be1
                               b2 <- evalBoolExp be2
                               return (b1 && b2)
evalBoolExp (Or be1 be2) = do b1 <- evalBoolExp be1
                              b2 <- evalBoolExp be2
                              return (b1 || b2)
evalBoolExp (Not be) = do b <- evalBoolExp be
                          return (not b)
