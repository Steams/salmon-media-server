{-# LANGUAGE OverloadedStrings #-}

module Salmon.Hls where

import           Path
import           Salmon.FileSystem
import           System.Directory
import           System.Process
import           Text.Printf



ffmpeg_generate mp3 hash =
  printf "ffmpeg -i '%s' -c:a aac -b:a 64k -vn -hls_list_size 0 '%s'" (toFilePath mp3) (playlist_path mp3 hash)

generate_art mp3 album =
  printf "ffmpeg -i '%s' -an -vcodec copy '%s' " (toFilePath mp3) (art_path mp3 album)

generate_playlist :: (Path Abs File,String,String) -> IO ()
generate_playlist (mp3, hash, album_name) = do
  bool <- doesFileExist (playlist_path mp3 hash)
  artbool <- doesFileExist (art_path mp3 album_name)
  case bool of
    True  -> return ()
    False -> mapM_ callCommand [ffmpeg_generate mp3 hash, if artbool then "echo 'exists'" else generate_art mp3 album_name]
