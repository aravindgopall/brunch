module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import Lib
import System.Directory
import System.FSNotify

main :: IO ()
main = do
  buildMapRecursiveAndWriteToFile
  watchAnd print onlyModifiedEvents "."

buildMapRecursiveAndWriteToFile :: IO ()
buildMapRecursiveAndWriteToFile = do
  cwd <- getCurrentDirectory
  allFiles <- listDirectory cwd
  traverse (buildMapAndWriteToFile cwd) allFiles
  putStrLn $ "building map is done in " <> cwd
  where
    buildMapAndWriteToFile cwd fp = do
      fMap <- buildMap fp
      let fileName = T.unpack $ last $ T.splitOn (T.pack "/") (T.pack fp)
      BS.writeFile fileName (encode fMap)

onlyModifiedEvents :: Event -> Bool
onlyModifiedEvents (Modified _ _ _) = True
onlyModifiedEvents _ = False
