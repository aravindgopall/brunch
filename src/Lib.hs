module Lib
  ( someFunc
  , watchAnd
  , buildMap
  , readPreviousMapper
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Mapper
import System.FSNotify

someFunc :: IO ()
someFunc = putStrLn "someFunc"

watchAnd fn pred dir =
  withManager $ \mgr -> do
    watchTree
      mgr -- manager
      dir -- directory to watch
      pred -- predicate
      fn -- action
    -- sleep forever (until interrupted)
    forever $ threadDelay 1000000
