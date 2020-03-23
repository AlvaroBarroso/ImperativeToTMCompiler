import Tape
import AST
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)
data Binary = Zero | One

newtype TM a = TM { runTM :: a}

instance Monad TM where
    return x    = TM x
    (TM t) >>=f = f t 

instance Functor TM where
    fmap = liftM

instance Applicative TM where
    pure   = return
    (<*>)  = ap

