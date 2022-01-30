{-# LANGUAGE TupleSections #-}

module MiniEff.State where

import MiniEff.Internal

data State a x where
  Put :: a -> State a ()
  Get :: State a a

put :: Member (State a) r => a -> Eff r ()
put = send . Put

get :: Member (State a) r => Eff r a
get = send Get

runState :: a -> Eff (State a : r) e -> Eff r (e, a)
runState initialState =
  handleRelayS
    initialState
    (\s a -> pure (a, s))
    ( \s0 msg k -> case msg of
        Get -> k s0 s0
        Put s1 -> k s1 ()
    )
