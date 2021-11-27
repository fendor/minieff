{-# LANGUAGE RankNTypes #-}
module MiniEff.Reader where

import MiniEff.Internal

data Reader i x where
  Get :: Reader i i

ask :: Member (Reader i) r => Eff r i
ask = send Get

asks :: Member (Reader i) r => (i -> i) -> Eff r i
asks f = do
  n <- ask
  pure $ f n

local :: forall i r a. Member (Reader i) r => (i -> i) -> Eff r a -> Eff r a
local f m = do
  x <- asks f
  -- Local signature is needed, as always with GADTs
  let h :: Reader i x -> (x -> Eff r a) -> Eff r a
      h Get g = g x
  interpose pure h m

runReader :: i -> Eff (Reader i : r) a -> Eff r a
runReader i = handleRelay Pure (\Get cont -> cont i)
{--
runReader :: i -> Eff (Reader i : r) a -> Eff r a
runReader i = loop
  where
    loop (Pure r) = Pure r
    loop (Impure u k) = case decomp u of
      Right Get -> loop $ k i
      Left o -> Impure o (loop . k)
--}
