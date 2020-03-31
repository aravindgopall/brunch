module Main where

import Lib
import System.FSNotify

main :: IO ()
main = watchAnd print onlyModifiedEvents "."

onlyModifiedEvents (Modified _ _ _) = True
onlyModifiedEvents _ = False
