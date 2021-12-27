module MiniEff.Coroutines where

import MiniEff.Internal

data Yield a b c where
  Yield :: a -> Yield a b b

data Status eff a b r
  = Done r
  | Continue a (b -> Eff eff (Status eff a b r))

yield :: Member (Yield a b) r => a -> Eff r b
yield val = send $ Yield val

replyC ::
  Yield a b c ->
  (c -> Eff effs (Status effs a b r)) ->
  Eff effs (Status effs a b r)
replyC (Yield a) mc =
  pure $ Continue a mc

interposeC ::
  Member (Yield a b) effs =>
  Eff effs r ->
  Eff effs (Status effs a b r)
interposeC = interpose (pure . Done) replyC

runC :: Eff (Yield a b : effs) r -> Eff effs (Status effs a b r)
runC = handleRelay (pure . Done) replyC

runLocal :: (a -> Eff effs b) -> Eff (Yield a b : effs) c -> Eff effs c
runLocal f =
  handleRelay pure (\(Yield a) cont -> f a >>= cont)

runLocalExit :: (a -> (c -> Eff effs b) -> Eff effs b) -> Eff (Yield a b : effs) c -> Eff effs c
runLocalExit f =
  handleRelay pure (\(Yield a) cont -> f a exit >>= cont)
  where
    exit = undefined

runLocalExit1 :: (a -> Eff effs (Either c b)) -> Eff (Yield a b : effs) c -> Eff effs c
runLocalExit1 f =
  handleRelay
    pure
    ( \(Yield a) cont ->
        f a >>= \case
          Left c -> pure c
          Right b -> cont b
    )

data Handler effs a b c = Handler (a -> Eff effs (Either c (b, Handler effs a b c)))

runLocalExit2 :: Handler effs a b c -> Eff (Yield a b : effs) c -> Eff effs c
runLocalExit2 _ (Pure c) = Pure c
runLocalExit2 (Handler f) (Impure u k) =
    case decomp u of
      Right (Yield a) -> f a >>= \case
        Left c -> pure c
        Right (b, f1) -> runLocalExit2 f1 $ k b
      Left o -> Impure o (runLocalExit2 (Handler f) . k)
