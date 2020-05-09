module Main where

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text            as T
import           Lib
import           System.Directory
import           System.FSNotify

main :: IO ()
main = do
  buildMapRecursiveAndWriteToFile
  watchAnd (compileUsingPurs <=< shouldCompileDepsOrNot) onlyModifiedEvents "."

buildMapRecursiveAndWriteToFile :: IO ()
buildMapRecursiveAndWriteToFile = do
  cwd <- getCurrentDirectory
  createDirectoryIfMissing False (cwd <> "/butch-output")
  allFiles <-
    filter (T.isInfixOf (T.pack ".purs") . T.pack) <$> listDirectory cwd
  traverse (buildMapAndWriteToFile cwd) allFiles
  putStrLn $ "building map is done in " <> cwd
  where
    buildMapAndWriteToFile cwd fp = do
      fMap <- buildMap fp
      let fileName =
            T.unpack $
            T.replace (T.pack ".purs") (T.pack ".json") $
            last $ T.splitOn (T.pack "/") (T.pack fp)
      BS.writeFile
        (cwd <> "/butch-output/" <> fileName)
        (BSL.toStrict $ encode fMap)

shouldCompileDepsOrNot :: Event -> IO (FilePath, Bool)
shouldCompileDepsOrNot (Modified fp time b) = do
  mapP <- readPreviousMapper fp
  mapC <- buildMap fp
  let fileName =
        T.unpack $
        T.replace (T.pack ".purs") (T.pack ".json") $ last $ T.splitOn (T.pack "/") (T.pack fp)
  cwd <- getCurrentDirectory
  BS.writeFile
    (cwd <> "/bunch-output/" <> fileName)
    (BSL.toStrict $ encode mapC)
  print mapC
  pure $ (fp, mapP == mapC)

compileUsingPurs :: (FilePath, Bool) -> IO ()
compileUsingPurs (fp, True)  = print fp -- compile only fp using purs
compileUsingPurs (fp, False) = print fp -- compile all the deps along with fp

onlyModifiedEvents :: Event -> Bool
onlyModifiedEvents (Modified _ _ _) = True
onlyModifiedEvents _                = False
