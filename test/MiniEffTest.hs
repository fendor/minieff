module MiniEffTest where

import Control.Monad (forM_)
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

producer :: Members [Reader Int, Yield Int ()] effs => Eff effs ()
producer =
  forM_ [1 .. 100] (yield @Int @())

consumer :: Members [Writer String, Writer Int, Reader Int, Yield () Int] effs => Int -> Eff effs ()
consumer n = do
  -- local @Int undefined $ tell @Int 1 -- = tell 1

  x <- ask @Int
  tell x
  tell $
    if n `mod` 3 == 0 && n `mod` 5 == 0
      then "FizzBuzz"
      else
        if n `mod` 3 == 0
          then "Fizz"
          else
            if n `mod` 5 == 0
              then "Buzz"
              else show n
  yield () >>= consumer

coroutine1 :: Members [Writer String, Reader Int, Yield Int String] effs => Eff effs ()
coroutine1 = do
  n <- ask @Int
  s <- yield @Int @String n
  tell s
  _ <- yield @Int @String 5000
  pure ()

coroutine2 :: Members [Writer String, Yield String Int] effs => Int -> Eff effs ()
coroutine2 n = do
  n2 <- yield @String @Int (show $ 1000 * n)
  tell $ show n2
  _ <- yield @String @Int (show $ 10000 * n2)
  pure ()

runCoroutineExample :: ((), [String])
runCoroutineExample = run $ runWriterList @String . runReader @Int 15 $ runC2 @Int @String coroutine2 coroutine1
