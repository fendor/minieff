module FreerSimple where

import Control.Monad.Freer
import Control.Monad.Freer.Coroutine
import Control.Monad.Freer.Reader
import Control.Monad.Freer.Writer

runExample ::  Eff [Reader Int, Writer [String]] Int -> (Int, [String])
runExample m = run $ runWriter @[String] $ runReader (15 :: Int) m

yieldWithLocal :: (Members [Reader Int, Writer [String]] effs) => Eff (Yield Int Int :effs) Int
yieldWithLocal = do
  x <- ask @Int
  tell ["Ask is: " ++ show x]
  y <- local @Int (+ 15) $ do
    n <- ask @Int
    tell ["Ask in local: " ++ show n]
    _ <- yield @Int @Int x show
    ask
  pure y

yieldWithoutLocal :: (Members [Reader Int, Writer [String]] effs) => Eff (Yield Int Int : effs) Int
yieldWithoutLocal = do
  x <- ask @Int
  tell ["Ask is: " ++ show x]
  _ <- yield @Int @Int x show
  ask

runWithYield :: Members [Writer [String], Reader Int] effs => Eff (Yield Int Int : effs) Int -> Eff effs Int
runWithYield action = do
  status <- runC @Int @Int action
  res <- case status of
    Done _ -> error "Impossible"
    Continue val cont -> do
      tell ["Yielded: " ++ show val]
      (local @Int (const 10) $ cont val) >>= \case
        Done r -> pure r
        Continue _ _ -> error "Impossible 2"
  pure res
