module Main (main) where

import Esper

main :: Effect (console :: CONSOLE, error :: ERROR, file :: FILE) Unit
main = do
  readFile "replays/F811C1D24888015E23B598AD8628C742.replay" throw \buffer -> do
    Tuple replay _offset <- runState (runReader getReplay buffer) (Offset 0)
    inspect replay
