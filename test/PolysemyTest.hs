module PolysemyTest where

import Polysemy
import Polysemy.Error
import Polysemy.Resource

resourceWithError :: Members [Embed IO, Resource, Error String] r => Sem r Int
resourceWithError = do
  bracket
    (pure (5 :: Int))
    (\_ -> embed (putStrLn "Exiting") >> pure (10 :: Int))
    ( \n -> do
        embed $ putStrLn "Run stuff"
        throw $ "Error with num: " ++ show n
    )

runStuff :: IO (Either String Int)
runStuff = runM $ runResource $ runError resourceWithError
