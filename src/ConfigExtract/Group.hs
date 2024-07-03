module ConfigExtract.Group where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString as DBS
import Data.ByteString.Lazy (fromStrict,toStrict)
import System.Directory
import System.FilePath
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe,mapMaybe)
import Data.List
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.List.Extra (replace,splitOn)
import System.Environment (lookupEnv)
import ConfigExtract.Types
import Data.Text (Text,pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8,decodeUtf8')

processDumpFileFieldUsage :: String -> FilePath -> IO (String,Map.Map Text CoreFunction)
processDumpFileFieldUsage baseDirPath path = do
  let module_name = replace ".hs.configExtract.ast.show.jsonL" ""
                      $ replace "/" "."
                        $ if (("src/")) `isInfixOf` (path)
                            then last (splitOn ("src/") (replace baseDirPath "" path))
                          else if (("src-generated/")) `isInfixOf` (path)
                              then last (splitOn ("src-generated/") (replace baseDirPath "" path))
                          else if (("src-extras/")) `isInfixOf` (path)
                              then last (splitOn ("src-extras/") (replace baseDirPath "" path))
                          else if (("test/")) `isInfixOf` (path)
                              then last (splitOn ("test/") (replace baseDirPath "" path))
                          else replace baseDirPath "" path
  print module_name
  content <- readFile path
  let (d :: [CoreFunction]) = concatMap (\x -> 
                                case Aeson.decode $ fromStrict $ encodeUtf8 $ pack x of
                                  Just (val :: CoreFunction) -> [val]
                                  Nothing -> []
                              ) $ lines content
      mapContent = Map.fromList $ map (\x -> (coreName x,x)) d
  pure (module_name, mapContent)

run :: Maybe String -> IO ()
run bPath = do
  let baseDirPath =
        case bPath of
            Just val -> val
            _ -> "/tmp/configExtract/"
  files <- getDirectoryContentsRecursive baseDirPath
  let jsonFiles = filter (\x -> (".configExtract.ast.show.jsonL" `isSuffixOf`) $ x) files
  fieldUsage <- mapM (processDumpFileFieldUsage baseDirPath) jsonFiles
  B.writeFile (baseDirPath <> "/" <> "configExtract-data.json") (toStrict $ encodePretty (Map.fromList fieldUsage))

getDirectoryContentsRecursive :: FilePath -> IO [FilePath]
getDirectoryContentsRecursive dir = do
    names <- listDirectory dir
    paths <- forM names $ \name -> do
        let path = dir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then getDirectoryContentsRecursive path
            else return [path]
    return (concat paths)
