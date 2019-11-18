{-# LANGUAGE OverloadedStrings #-}

module Salmon.Server(run_server,url_path) where

import           Network.Wai.Middleware.Cors
import           Salmon.FileSystem
import           Web.Scotty

url_path :: [Char] -> [Char]
url_path mp3 = "http://localhost:3000/" ++ (take (length mp3 - 4) mp3) ++ ".m3u8"

serve_files :: ScottyM ()
serve_files =
  get "/:file" $ do
    file_name <- param "file"
    file $ working_dir_path ++ file_name

run_server :: IO ()
run_server = scotty 3000 $ do
  middleware simpleCors
  serve_files
