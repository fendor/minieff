module MiniEff.Coroutines where

import MiniEff.Internal

data Yield a b c where
  Yield :: a -> (b -> c) -> Yield a b c

data Status eff a b r
  = Done r
  | Continue a (b -> Eff eff (Status eff a b r))

yield :: Member (Yield a b) r => a -> (b -> c) -> Eff r c
yield val cont = send $ Yield val cont

replyC ::
  Yield a b c ->
  (c -> Eff effs (Status effs a b r)) ->
  Eff effs (Status effs a b r)
replyC (Yield a f) mc =
  pure $ Continue a (mc . f)

interposeC ::
  Member (Yield a b) effs =>
  Eff effs r ->
  Eff effs (Status effs a b r)
interposeC = interpose (pure . Done) replyC

runC :: Eff (Yield a b : effs) r -> Eff effs (Status effs a b r)
runC = handleRelay (pure . Done) replyC
