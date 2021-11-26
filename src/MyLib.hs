{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module MyLib where

import Data.OpenUnion
import Control.Monad ((>=>))

-- data Free f a = Pure a | Impure (f (Free f a))

-- instance Functor f => Functor (Free f) where
--   fmap f (Pure a) = Pure (f a)
--   fmap f (Impure k) = Impure (fmap (fmap f) k)

-- instance Functor f => Applicative (Free f) where
--   pure = Pure
--   Pure a <*> xs = fmap a xs
--   Impure f <*> xs = Impure (fmap (<*> xs) f)

-- instance Functor f => Monad (Free f) where
--   Pure a >>= k = k a
--   Impure f >>= k = Impure (fmap (>>= k) f)

-- data FFree f a where
--   Pure :: a -> FFree f a
--   Impure :: f x -> (x -> FFree f a) -> FFree f a

-- data FReaderWriter i o x where
--   Get :: FReaderWriter i o i
--   Put :: o -> FReaderWriter i o ()

-- type IT i o a = FFree (FReaderWriter i o) a

-- instance Functor (FFree f) where
--   fmap f (Pure a) = Pure (f a)
--   fmap f (Impure g cont) = Impure g (fmap (fmap f) cont)

-- instance Applicative (FFree f) where
--   pure = Pure
--   Pure f <*> xs = fmap f xs
--   Impure g cont <*> xs = Impure g (fmap (<*> xs) cont)

-- instance Monad (FFree f) where
--   Pure x >>= k = k x
--   Impure fx k' >>= k = Impure fx (k' >=> k)

data Lan (g :: * -> *) a where
  FMap :: (x -> a) -> g x -> Lan g a

instance Functor (Lan g) where
  fmap h (FMap h' gx) = FMap (h . h') gx

-- type FFRee g = FFree (Lan g)

data BiFree p a b where
  Bimap :: (a -> b) -> (c -> d) -> p a c -> BiFree p b d



--

data Eff r a where
  Pure :: a -> Eff r a
  Impure :: Union r x -> (x -> Eff r a) -> Eff r a

instance Functor (Eff r) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Impure fx cont) = Impure fx (fmap (fmap f) cont)

instance Applicative (Eff r) where
  pure = Pure
  Pure f <*> xs = fmap f xs
  Impure fx cont <*> xs = Impure fx (fmap (<*> xs) cont)

instance Monad (Eff r) where
  Pure x >>= f = f x
  Impure fx k' >>= k = Impure fx (k' >=> k)

type Arr r a b = a -> Eff r b
