{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Salmon(run) where

import           Control.Concurrent       (forkIO, threadDelay)
import           Control.Concurrent.Async (mapConcurrently)
import           Control.Monad            (forever)
import           Data.List
import           Data.Set                 as Set hiding (filter, map)
import           Network.HTTP.Simple
import           Path
import           Salmon.Data
import           Salmon.FileSystem
import           Salmon.Hls
import           Salmon.Hub               as Hub
import           Salmon.Server
import           System.FSNotify


synch_with_hub :: Credentials -> IO ()
synch_with_hub credentials = do
  local_files            <- get_files
  local_hashes           <- mapM to_hash local_files
  remote_hashes_response <- Hub.get_hashes credentials
  let remote_hashes       = getResponseBody remote_hashes_response
  let new_hashes          = difference ( Set.fromList local_hashes  ) ( Set.fromList remote_hashes )
  let deleted_hashes      = difference ( Set.fromList remote_hashes ) ( Set.fromList local_hashes  )
  let new_files           = filter (\(file,hash) -> elem hash new_hashes) (zip local_files local_hashes)
  new_tracks             <- mapConcurrently parse_track (map fst new_files)
  mapM_ print new_tracks
  mapM_ print deleted_hashes
  let new_media           = zipWith track_to_media new_files new_tracks
  response               <- Hub.post_songs credentials new_media
  print . getResponseStatus $ response
  response               <- Hub.delete_songs credentials (toList deleted_hashes)
  print . getResponseStatus $ response


initialize_playlists :: IO ()
initialize_playlists = do
  files <- get_files
  create_working_dir
  mapM_ (forkIO . generate_playlist) files


watch_predicate :: Event -> Bool
watch_predicate =
  \case
    Added path _ _ ->
      if isSuffixOf ".mp3" path
        then True
        else False
    Removed path _ _ ->
      if isSuffixOf ".mp3" path
        then True
        else False
    Modified _ _ _ -> False
    Unknown _ _ _ -> False


process_update :: Credentials -> Event -> IO ()
process_update credentials =
  \case
    Added path _ _ -> do
      file_path    <- parseAbsFile path
      threadDelay 1000000
      hash         <- to_hash file_path
      track        <- parse_track file_path
      putStr "Printing prased track :: "
      print track
      let media     = track_to_media (file_path, hash) track
      generate_playlist file_path
      response     <- Hub.post_songs credentials [media]
      print . getResponseStatus $ response

    Removed _ _ _ -> do
      local_files  <- get_files
      local_hashes <- mapM to_hash local_files
      res          <- Hub.get_hashes credentials
      let remote_hashes  = getResponseBody res
      let deleted_hashes = difference (Set.fromList remote_hashes) (Set.fromList local_hashes)
      mapM_ print deleted_hashes
      response     <- Hub.delete_songs credentials (toList deleted_hashes)
      print . getResponseStatus $ response

    Modified _ _ _ -> return ()
    Unknown _ _ _ -> return ()

watch_fs :: Credentials -> IO a
watch_fs credentials =
  withManager $ \mgr -> do
    _ <- watchDir mgr library_path watch_predicate (process_update credentials)
    forever $ threadDelay 1000000

run :: String -> String -> IO ()
run username password = do
  credentials <- Hub.login username password
  putStr "CREDENTIALS :"
  print credentials
  initialize_playlists
  synch_with_hub credentials
  putStrLn "settup complete"
  _ <- forkIO $ watch_fs credentials
  _ <- forkIO run_server
  forever $ threadDelay 1000000
