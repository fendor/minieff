module Example where

import MiniEff

double :: Member (Reader Int) r => Eff r Int
double = do
  x <- ask @Int
  y <- ask @Int
  pure $ x + y

someWriterTest :: Member (Writer String) r => Eff r Int
someWriterTest = do
  tell "Start"
  (_, msgs) <- listen @String act
  tellAll msgs
  tell $ "listened: " ++ show (length msgs)
  pure 0
  where
    act = do
      tell "this is a message"
      tell "more messages"

yieldWithLocal :: (Members [Reader Int, Writer String, Yield Int Int] effs) => Eff effs Int
yieldWithLocal = do
  x <- ask @Int
  tell $ "Ask is: " ++ show x
  y <- local @Int (+ 15) $ do
    n <- ask @Int
    tell $ "Ask in local: " ++ show n
    _ <- yield @Int @Int x show
    ask
  pure y

yieldWithoutLocal :: (Members [Reader Int, Writer String, Yield Int Int] effs) => Eff effs Int
yieldWithoutLocal = do
  x <- ask @Int
  tell $ "Ask is: " ++ show x
  n <- ask @Int
  tell $ "Ask in local: " ++ show n
  _ <- yield @Int @Int x show
  ask

runWithYield :: Members [Writer String, Reader Int] effs => Eff effs Int
runWithYield = do
  status <- runC @Int @Int yieldWithLocal
  res <- case status of
    Done _ -> error "Impossible"
    Continue val cont -> do
      tell $ "Yielded: " ++ show val
      (local @Int (const 10) $ cont val) >>= \case
        Done r -> pure r
        Continue _ _ -> error "Impossible 2"
  pure res

-- weirdThingy :: Members [Reader Int, Writer String, Yield Int Int] effs => Eff effs Int
-- weirdThingy = do
--   x <- ask @Int
--   status <- interposeC $ do
--     num <- yield (2 :: Int) show
--     tell num

--   case status of
--     Continue a cont -> cont a
--     Done r -> error "Fail!"

--   pure x
