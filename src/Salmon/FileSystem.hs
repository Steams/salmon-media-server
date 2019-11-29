module Salmon.FileSystem where

import qualified Data.ByteString.Lazy as ByteString
import           Data.Digest.Pure.MD5
import           Data.List
import           Data.List.Split
import           Path
import           System.Directory


type Folder = Path Abs Dir
type File_ = Path Abs File

working_dir_path folder = do
  working_dir <- parseRelDir ".salmon"
  return $ folder </> working_dir

-- TODO this should probably return a Abs file path aswell, not a filepath string as it does now
playlist_path :: File_ -> String
playlist_path mp3 =
  (toFilePath . parent $ mp3) ++ ".salmon/" ++  name ++ ".m3u8"
  where
    name = take (length (get_file_name mp3) - 4) (get_file_name mp3)

get_file_name :: Path b File -> String
get_file_name = toFilePath . filename

to_hash :: Path b File -> IO String
to_hash path = do
  file_data     <- ByteString.readFile $ toFilePath path
  return $ show $ md5 file_data

get_files :: Folder -> IO [File_]
get_files folder = do
  files           <- getDirectoryContents (toFilePath folder)
  let full_paths  =  map (toFilePath folder ++) files
  let mp3s        =  filter (isSuffixOf ".mp3") full_paths
  parsed          <- mapM parseAbsFile mp3s
  return parsed

create_working_dir :: Folder -> IO ()
create_working_dir folder = do
  working_dir <- working_dir_path folder
  bool <- doesDirectoryExist $ toFilePath working_dir
  case bool of
    True  -> return ()
    False -> createDirectory $ toFilePath working_dir

get_abs_path :: FilePath -> IO Folder
get_abs_path path = do
  abs <- makeAbsolute path
  dir <- parseAbsDir abs
  return dir


get_abs_file :: FilePath -> IO File_
get_abs_file path = do
  abs <- makeAbsolute path
  file <- parseAbsFile abs
  return file
