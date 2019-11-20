module Salmon.FileSystem where

import qualified Data.ByteString.Lazy as ByteString
import           Data.Digest.Pure.MD5
import           Data.List
import           Data.List.Split
import           Path
import           System.Directory


library_path :: [Char]
library_path = "/home/steams/Development/audigo/salmon-media-server/resources/"

working_dir_path :: [Char]
working_dir_path =
  "/home/steams/Development/audigo/salmon-media-server/resources/.salmon/"

playlist_path :: [Char] -> [Char]
playlist_path mp3 = working_dir_path ++ (take (length mp3 - 4) mp3) ++ ".m3u8"

get_file_name :: Path b File -> FilePath
get_file_name = (toFilePath . filename)

to_hash :: Path b File -> IO String
to_hash path = do
  file_data     <- ByteString.readFile $ toFilePath path
  return $ show $ md5 file_data

get_files :: IO [Path Abs File]
get_files = do
  files           <- getDirectoryContents library_path
  let full_paths  =  map ((++) library_path) files
  let mp3s        =  filter (isSuffixOf ".mp3") full_paths
  parsed          <- mapM parseAbsFile mp3s
  return parsed

create_working_dir :: IO ()
create_working_dir = do
  bool <- doesDirectoryExist working_dir_path
  case bool of
    True  -> return ()
    False -> createDirectory working_dir_path
