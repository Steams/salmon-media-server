{-# LANGUAGE OverloadedStrings #-}

module Salmon.Server(run_server,url_path, art_url) where

import           Network.Wai.Middleware.Cors
import           Salmon.FileSystem
import           Web.Scotty
import           Path

url_path :: [Char] -> [Char]
url_path hash = "http://localhost:3000/" ++ hash ++ ".m3u8"

art_url :: [Char] -> [Char]
art_url album = "http://localhost:3000/" ++ album ++ ".jpg"

serve_files :: Folder -> ScottyM ()
serve_files folder =
  get "/:file" $ do
    file_name <- param "file"
    working_dir <- working_dir_path folder
    file $ toFilePath  working_dir ++ file_name

run_server :: Folder -> IO ()
run_server folder = scotty 3000 $ do
  middleware simpleCors
  serve_files folder
