module PolysemyTest where

import Polysemy
import Polysemy.Error
import Polysemy.Resource
import Polysemy.State

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

foo :: Members [Error String, State Int] r => Sem r Int
foo = do
  catch
    ( do
        put @Int 5
        _ <- throw "String"
        pure 10
    )
    ( \_ -> pure 0
    )
