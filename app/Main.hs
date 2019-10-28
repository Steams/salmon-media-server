{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.List
import           Data.Maybe
import           Data.Monoid      (mconcat)
import           Lib
import           System.Directory
import           System.Process
import           Text.Printf
import           Web.Scotty


-- data MediaStatus = New | Synched | Deleted
-- data Media = Media {path :: String, status :: MediaStatus}

-- data Report = Report {update :: Update, status :: Either Error}

-- data Update = Remove Media | Add Media

-- media_name :: Media -> String
-- media_name = undefined

-- process_media media = do
--   created <- mapM update_create $ filter (\x -> status x == New) media
--   deleted <- mapM update_delete $ filter (\x -> status x == Deleted) media
--   push_update $ batch_updates c d

--     -- maybe scan for updates should return array of updates which is a list of New or Deleted
--     -- process updates generates playlists for new, and deletes files for deleted
--     -- process updates returns a list of reports, which is the update and info about the success/failure

-- main :: IO ()
-- main = do
--   status <- check_status
--   case status of
--     Initialized -> scan_for_updates >>= process_media
--     Uninitialzied -> create_working_dir >> get_all_media >>= process_media
--   watch_fs

playlist_path mp3 = working_dir_path ++ "/" ++ (take (length mp3 - 4) mp3) ++ ".m3u8"

ffmpeg_generate mp3 =
  printf "ffmpeg -i %s -c:a aac -b:a 64k -vn -hls_list_size 0 %s" mp3_path playlist
  where
    playlist = playlist_path mp3
    mp3_path = dir_path ++ "/" ++ mp3

dir_path = "/home/steams/Development/audigo/salmon-media-server/resources"
working_dir_path = "/home/steams/Development/audigo/salmon-media-server/resources/.salmon"

get_files :: IO [[Char]]
get_files = filter (isSuffixOf ".mp3") <$> getDirectoryContents dir_path

generate_playlist :: String -> IO ()
generate_playlist mp3 = do
  bool <- doesFileExist $ playlist_path mp3
  case bool of
    True  -> return ()
    False -> callCommand $ ffmpeg_generate mp3

create_working_dir = do
  bool <- doesDirectoryExist working_dir_path
  case bool of
    True  -> return ()
    False -> createDirectory working_dir_path

file_route =
  get "/:file" $ do
    file_name <- param "file"
    file $ working_dir_path ++ "/" ++ file_name

run_server = scotty 3000 $ do file_route

main = do
  create_working_dir
  files <- get_files
  mapM_ generate_playlist files
  run_server
