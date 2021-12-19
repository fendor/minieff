{-# LANGUAGE RankNTypes #-}

module MiniEff.Internal
  ( module MiniEff.Internal,
    module Data.OpenUnion,
  )
where

import Control.Monad ((>=>))
import Data.OpenUnion

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

run :: Eff '[] a -> a
run = \case
  Pure a -> a
  Impure _ _ -> error "Impossible"

send :: Member t r => t v -> Eff r v
send t = Impure (inj t) Pure

handleRelay ::
  (a -> Eff r b) ->
  (forall v. t v -> (v -> Eff r b) -> Eff r b) ->
  Eff (t : r) a ->
  Eff r b
handleRelay ret _ (Pure b) = ret b
handleRelay ret h (Impure u k) =
  case decomp u of
    Right x -> h x (handleRelay ret h . k)
    Left o -> Impure o (handleRelay ret h . k)

interpose ::
  Member t r =>
  (a -> Eff r b) ->
  (forall v. t v -> (v -> Eff r b) -> Eff r b) ->
  Eff r a ->
  Eff r b
interpose ret _ (Pure b) = ret b
interpose ret h (Impure u k) =
  case prj u of
    Just x -> h x (interpose ret h . k)
    Nothing -> Impure u (interpose ret h . k)

raise :: Eff r a -> Eff (e ': r) a
raise = loop
  where
    loop (Pure x) = pure x
    loop (Impure u q) = Impure (weaken u) (loop . q)

replace :: (a -> Eff (v : r) w) -> (forall x. t x -> (x -> Eff (v : r) w) -> Eff (v : r) w) -> Eff (t : r) a -> Eff (v : r) w
replace pure' bind = loop
  where
    loop (Pure x) = pure' x
    loop (Impure u' q) = case decomp u' of
      Right x -> bind x (loop . q)
      Left u -> Impure (weaken u) (loop . q)
