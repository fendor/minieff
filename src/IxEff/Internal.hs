{-# LANGUAGE RebindableSyntax #-}
module IxEff.Internal where

import Prelude hiding (Monad(..))
import qualified Prelude as P
import Data.Functor.Identity

class IxMonad m where
    return :: a -> m p p a
    (>>=) :: m p q a -> (a -> m q r b) -> m p r b
    (>>) :: m p q a -> m q r b -> m p r b

    f >> g = f >>= \_ -> g

-- Example: State Monad

newtype IxStateT m p q a = IxStateT { runIxStateT :: p -> m (q, a) }

instance P.Monad m => IxMonad (IxStateT m) where
    return a = IxStateT (\p -> pure (p, a))
    s >>= f =
        IxStateT $ \p ->
            runIxStateT s p P.>>= \(q, a) ->
            runIxStateT (f a) q

            -- (r, b) <- runIxStateT (f a) q
            -- pure (r, b)

ixput :: P.Monad m => q -> IxStateT m p q ()
ixput q = IxStateT $ \_ -> pure (q, ())

ixget :: P.Monad m => IxStateT m p p p
ixget = IxStateT $ \p -> pure (p, p)

-- State changer example:

foo :: IxStateT Identity Int Bool String
foo = do
    n <- ixget
    ixput "Test"
    x <- ixget
    ixput True
    return $ show n ++ x


