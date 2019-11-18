module Salmon.FileSystem where

import           Data.List
import           System.Directory

dir_path :: [Char]
dir_path = "/home/steams/Development/audigo/salmon-media-server/resources/"

working_dir_path :: [Char]
working_dir_path =
  "/home/steams/Development/audigo/salmon-media-server/resources/.salmon/"

playlist_path :: [Char] -> [Char]
playlist_path mp3 = working_dir_path ++ (take (length mp3 - 4) mp3) ++ ".m3u8"

get_files :: IO [String]
get_files = filter (isSuffixOf ".mp3") <$> getDirectoryContents dir_path

create_working_dir :: IO ()
create_working_dir = do
  bool <- doesDirectoryExist working_dir_path
  case bool of
    True  -> return ()
    False -> createDirectory working_dir_path
