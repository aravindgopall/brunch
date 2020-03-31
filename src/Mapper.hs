module Mapper where

import Data.Hashable
import Data.List hiding (insert)
import Data.Map hiding (filter, foldl, intercalate, map, split)
import qualified Data.Text as T

buildMap :: FilePath -> IO (Map String Int)
buildMap filePath = do
  fileData <- lines <$> readFile filePath
  -- TODO: either use parser or any other way to correctly get the fnDefs
  let fnDefs = filter (isInfixOf "::") fileData
  return $ foldl accumalateTheMap empty fnDefs
  where
    accumalateTheMap :: Map String Int -> String -> Map String Int
    accumalateTheMap m l =
      let l' = map T.unpack $ T.splitOn (T.pack "::") (T.pack l)
          fnName = filter ((/=) ' ') $ l' !! 0
          fnSignA =
            if length l' > 2
              then intercalate "" $ tail l'
              else l' !! 1
          fnSign =
            filter ((/=) ' ') $
            intercalate "" $
            map T.unpack $ T.splitOn (T.pack "->") (T.pack fnSignA)
       in insert fnName (hash fnSign) m
