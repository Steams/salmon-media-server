module Salmon(run) where

import           Network.HTTP.Simple
import           Salmon.Data
import           Salmon.FileSystem
import           Salmon.Hub
import           Salmon.Server
import           Salmon.Hls

synch_with_hub :: IO ()
synch_with_hub = do
    files     <- get_files
    tracks    <- mapM parse_track files
    media     <- return $ zipWith track_to_media files tracks
    responses <- mapM post_song media
    mapM_ print tracks
    mapM_ (print . getResponseStatus) responses

initialize_playlists :: IO ()
initialize_playlists = do
  files <- get_files
  create_working_dir
  mapM_ generate_playlist files

run :: IO ()
run = do
  initialize_playlists
  synch_with_hub
  putStrLn "settup complete"
  run_server
