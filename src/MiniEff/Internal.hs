{-# LANGUAGE RankNTypes #-}

module MiniEff.Internal
  ( module MiniEff.Internal,
    module Data.OpenUnion,
  )
where

import Control.Monad ((>=>))
import Data.OpenUnion

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

handleRelayS ::
  -- | Initial state
  s ->
  -- | Base case
  (s -> a -> Eff r b) ->
  (forall v. s -> t v -> (s -> v -> Eff r b) -> Eff r b) ->
  Eff (t : r) a ->
  Eff r b
handleRelayS s0 ret _ (Pure b) = ret s0 b
handleRelayS s0 ret h (Impure u k) = case decomp u of
  Right x -> h s0 x k'
  Left o -> Impure o (k' s0)
  where
    k' s1 x = handleRelayS s1 ret h (k x)

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
raise (Pure x) = pure x
raise (Impure u k) = Impure (weaken u) (raise . k)

replace ::
  (a -> Eff (v : r) w) ->
  (forall x. t x -> (x -> Eff (v : r) w) -> Eff (v : r) w) ->
  Eff (t : r) a ->
  Eff (v : r) w
replace ret _ (Pure x) = ret x
replace ret h (Impure u' k) =
  case decomp u' of
    Right x -> h x (replace ret h . k)
    Left u -> Impure (weaken u) (replace ret h . k)
