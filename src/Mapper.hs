module Mapper where

import Data.Hashable
import Data.List hiding (insert)
import Data.Map hiding (filter, foldl, intercalate, map, split)
import qualified Data.Text as T

buildMap :: FilePath -> IO (Map String Int)
buildMap filePath = do
  fileData <- lines <$> readFile filePath
  -- TODO: either use parser or any other way to correctly get the fnDefs
  let fns =
        filter
          (\x ->
             not
               (isInfixOf "import" x ||
                isInfixOf "module" x ||
                isInfixOf "where" x || isInfixOf "<-" x || x == ""))
          fileData
      (_, _, fnDefs) =
        foldl
          (\(pf, accP, accA) x ->
             if pf /= "" && isInfixOf pf x
               then ("", "", accP : accA)
               else if isInfixOf "::" x && pf == ""
                      then ( filter ((/=) ' ') $
                             (T.unpack $
                              head $ T.splitOn (T.pack "::") (T.pack x))
                           , x
                           , accA)
                      else if pf == ""
                             then getTriplet x accP accA
                             else (pf, accP <> x, accA))
          ("", "", [])
          fns
  return $ foldl accumalateTheMap empty fnDefs
  where
    getTriplet x accP accA =
      let allWords = words x
       in if length allWords /= 1
            then ("", accP, accA)
            else if accP == "" && not (isInfixOf "}" (head allWords))
                   then ( filter ((/=) ' ') $
                          (T.unpack $ head $ T.splitOn (T.pack "::") (T.pack x))
                        , x
                        , accA)
                   else ("", accP, accA)
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
