{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Salmon(run) where

import           Control.Concurrent        (forkIO, threadDelay)
import           Control.Concurrent.Async (mapConcurrently)
import           Control.Monad             (forever)
import qualified Data.ByteString.Lazy      as LB
import           Data.Digest.Pure.MD5
import           Data.List
import           Data.Set                  hiding (filter, isSuffixOf, map)
import           Network.HTTP.Simple
import           Path
import           Salmon.Data
import           Salmon.FileSystem
import           Salmon.Hls
import           Salmon.Hub                as Hub
import           Salmon.Server
import           System.FSNotify

synch_with_hub :: IO ()
synch_with_hub = do
  local_files            <- get_files
  local_hashes           <- mapM to_hash local_files
  remote_hashes_response <- Hub.get_hashes
  let remote_hashes      =  getResponseBody remote_hashes_response
  let new_hashes         =  difference ( fromList local_hashes  ) ( fromList remote_hashes )
  let deleted_hashes     =  difference ( fromList remote_hashes ) ( fromList local_hashes  )
  let new_files          =  filter (\(file,hash) -> elem hash new_hashes) (zip local_files local_hashes)
  new_tracks             <- mapConcurrently parse_track (map fst new_files)
  mapM_ print new_tracks
  mapM_ print deleted_hashes
  let new_media          =  zipWith track_to_media new_files new_tracks
  response               <- post_songs new_media
  print . getResponseStatus $ response
  response               <- delete_songs (toList deleted_hashes)
  print . getResponseStatus $ response


initialize_playlists :: IO ()
initialize_playlists = do
  files <- get_files
  create_working_dir
  mapM_ (forkIO . generate_playlist) files


watch_predicate =
  \case
    Added path time _ ->
      if isSuffixOf ".mp3" path
        then True
        else False

    Removed path time _ ->
      if isSuffixOf ".mp3" path
        then True
        else False
    _ -> False


process_update  = \case
    Added path time _ -> do
      file_path    <- parseAbsFile path
      threadDelay 1000000
      hash         <- to_hash file_path
      track        <- parse_track file_path
      putStr "Printing prased track :: "
      print track
      let media = track_to_media (file_path,hash) track
      generate_playlist file_path
      response     <- post_songs [media]
      print . getResponseStatus $ response

    Removed path time _ -> do
      local_files  <- get_files
      local_hashes <- mapM to_hash local_files
      res          <- Hub.get_hashes
      let remote_hashes   = getResponseBody res
      let deleted_hashes  =  difference ( fromList remote_hashes ) ( fromList local_hashes  )
      mapM_ print deleted_hashes
      response     <- delete_songs (toList deleted_hashes)
      print . getResponseStatus $ response

watch_fs =
  withManager $ \mgr -> do
    watchDir mgr library_path watch_predicate process_update
    forever $ threadDelay 1000000

run :: IO ()
run = do
  initialize_playlists
  synch_with_hub
  putStrLn "settup complete"
  watch_fs
  -- run_server
