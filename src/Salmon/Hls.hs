{-# LANGUAGE OverloadedStrings #-}

module Salmon.Hls where

import           Salmon.FileSystem
import           Path
import           System.Directory
import           System.Process
import           Text.Printf



ffmpeg_generate mp3 =
  printf "ffmpeg -i %s -c:a aac -b:a 64k -vn -hls_list_size 0 %s" (toFilePath mp3) (playlist_path mp3)

generate_playlist :: Path Abs File -> IO ()
generate_playlist mp3 = do
  bool <- doesFileExist (playlist_path mp3)
  case bool of
    True  -> return ()
    False -> callCommand $ ffmpeg_generate mp3
