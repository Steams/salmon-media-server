{-# LANGUAGE OverloadedStrings #-}

module Salmon.Hls where

import           Salmon.FileSystem
import           Path
import           System.Directory
import           System.Process
import           Text.Printf


ffmpeg_generate :: PrintfType t => [Char] -> t
ffmpeg_generate mp3 =
  printf "ffmpeg -i %s -c:a aac -b:a 64k -vn -hls_list_size 0 %s" mp3_path playlist
  where
    playlist = playlist_path mp3
    mp3_path = library_path ++ mp3

generate_playlist :: Path Abs File -> IO ()
generate_playlist mp3 = do
  bool <- doesFileExist $ playlist_path (get_file_name mp3)
  case bool of
    True  -> return ()
    False -> callCommand $ ffmpeg_generate (get_file_name mp3)
