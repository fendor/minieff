{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FlexibleInstances #-}

module IxEff.Internal where

import Data.Functor.Identity
import Prelude hiding (Monad (..), Applicative(..))
import qualified Prelude as P
import qualified Control.Monad as M
import qualified Control.Applicative as A

class IxApplicative m => IxMonad m where
  return :: a -> m p p a
  (>>=) :: m p q a -> (a -> m q r b) -> m p r b
  (>>) :: m p q a -> m q r b -> m p r b
  f >> g = f >>= \_ -> g

class IxApplicative m where
  pure :: a -> m p p a
  (<*>) :: m p q (a -> b) -> m q r a -> m p r b

class IxAlternative f where
  empty :: f p q a
  (<|>) :: f p q a -> f p q a -> f p q a

-- Example: State Monad

newtype IxStateT m p q a = IxStateT {runIxStateT :: p -> m (q, a)}

type StateT s m a = IxStateT m s s a

instance (P.Applicative m, M.Monad m) => IxApplicative (IxStateT m) where
  pure a = IxStateT (\p -> P.pure (p, a))
  IxStateT sf <*> IxStateT sx = IxStateT $ \p ->
    sf p P.>>= \(q, f) ->
    sx q P.>>= \(r, a) ->
    P.pure (r, f a)

instance M.Monad m => IxMonad (IxStateT m) where
  return a = IxStateT (\p -> P.pure (p, a))
  s >>= f =
    IxStateT $ \p ->
      runIxStateT s p P.>>= \(q, a) ->
        runIxStateT (f a) q

instance A.Alternative m => IxAlternative (IxStateT m) where
  empty = IxStateT $ \_ -> A.empty
  a <|> b = IxStateT $ \ s ->
    runIxStateT a s A.<|> runIxStateT b s

ixput :: A.Applicative m => q -> IxStateT m p q ()
ixput q = IxStateT $ \_ -> P.pure (q, ())

ixget :: A.Applicative m => IxStateT m p p p
ixget = IxStateT $ \p -> P.pure (p, p)

instance Functor m => Functor (IxStateT m p q) where
  fmap f (IxStateT a) = IxStateT (fmap (fmap (fmap f)) a)

instance (A.Applicative m, M.Monad m) => A.Applicative (IxStateT m p p) where
  pure x = A.pure x
  mf <*> mx = mf <*> mx

instance (A.Applicative m, M.Monad m) => M.Monad (IxStateT m p p) where
  mf >>= mx = mf >>= mx

-- Example: Reader Monad

-- newtype IxReaderT m p q a = IxReaderT {runIxReaderT :: p -> m a}

-- -- instance M.Monad m => IxMonad (IxReaderT m) where
-- --   return a = IxReaderT (\_ -> P.pure a)
-- --   s >>= f =
-- --     IxReaderT $ \p ->
-- --       runIxReaderT s p P.>>= \a ->
-- --         runIxReaderT (f a) undefined

-- -- ixask :: M.Monad m => IxReaderT m p q p
-- -- ixask = IxReaderT $ \p -> P.pure p

-- -- ixlocal :: (p -> r) -> IxReaderT m r q a -> IxReaderT m p q a
-- -- ixlocal f act = IxReaderT $ \p ->
-- --   runIxReaderT act (f p)

-- State changer example:

foo :: IxStateT Identity Int Bool String
foo = do
  n <- ixget
  ixput "Test"
  x <- ixget
  ixput True
  return $ show n ++ x

liftIxState :: P.Monad m => IxStateT m p q r -> IxStateT (IxStateT m p q) (s, p) (t, q) r
liftIxState act = IxStateT $ \s -> undefined

-- bar :: IxStateT (IxStateT Identity Double Bool) Int [Bool] String
-- bar = do
--   n <- ixget
