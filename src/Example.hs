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

