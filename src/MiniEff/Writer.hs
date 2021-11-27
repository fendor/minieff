{-# LANGUAGE TupleSections #-}

module MiniEff.Writer where

import MiniEff.Internal

data Writer w x where
  Tell :: w -> Writer w ()

tell :: Member (Writer w) r => w -> Eff r ()
tell = send . Tell

tellAll :: Member (Writer w) r => [w] -> Eff r ()
tellAll xs = mapM_ tell xs

listen ::
  forall w a effs.
  Member (Writer w) effs =>
  Eff effs a ->
  Eff effs (a, [w])
listen m = do
  let h :: Writer w v -> (v -> Eff effs (a, [w])) -> Eff effs (a, [w])
      h (Tell w) k = k () >>= \(x, i) -> pure (x, w : i)
  interpose
    (pure . (,[]))
    h
    m

runWriterList :: Eff (Writer w : r) x -> Eff r (x, [w])
runWriterList =
  handleRelay
    (pure . (,[]))
    ( \(Tell w) k ->
        k ()
          >>= \(x, i) -> pure (x, w : i)
    )
