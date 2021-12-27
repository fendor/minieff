module MiniEffTest where

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
    _ <- yield @Int @Int x
    ask
  pure y

yieldWithoutLocal :: (Members [Reader Int, Writer String, Yield Int Int] effs) => Eff effs Int
yieldWithoutLocal = do
  x <- ask @Int
  tell $ "Ask is: " ++ show x
  _ <- yield @Int @Int x
  -- asert: x1=x+1
  n <- ask @Int
  -- unreachable
  tell $ "Ask without local: " ++ show n
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

-- runWithCBYield :: Members [Writer String, Reader Int] effs => Eff effs Int
-- runWithCBYield = do
--   status <- runLocalExit1 @Int (\a -> do
--     case (a == 0) of
--       False -> pure $ Left @Int 0
--       True -> do
--         tell ("Yielded: " ++ show a)
--         pure $ Right $ a * 2
--     ) yieldWithLocal
--   status1 <- runLocalExit1 @Int ((\a -> pure $ Right $ a + 1) `firstHandlerOnceAndThenOtherHandler` (const $ pure $ Left (0 :: Int))) yieldWithLocal

--   pure status



-- task: coroutinen-server, der prefix-sums responded



-- weirdThingy :: Members [Reader Int, Writer String, Yield Int Int] effs => Eff effs Int
-- weirdThingy = do
--   x <- ask @Int
--   status <- interposeC $ do
--     num <- yield (2 :: Int) show
--     tell n
--   case status of
--     Continue a cont -> cont a
--     Done r -> error "Fail!"

--   pure x
