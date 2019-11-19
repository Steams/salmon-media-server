{-# LANGUAGE ScopedTypeVariables #-}
module Salmon(run) where

import qualified Data.ByteString.Lazy as LB
import           Data.Digest.Pure.MD5
import           Data.Set hiding (map,filter)
import           Network.HTTP.Simple
import           Salmon.Data
import           Salmon.FileSystem
import           Salmon.Hls
import           Salmon.Hub
import           Salmon.Server

synch_with_hub :: IO ()
synch_with_hub = do
  local_files         <- get_files
  local_file_data     <- mapM LB.readFile $ map get_path local_files
  local_hashes        <- return $ map md5 local_file_data
  res                 <- get_hashes
  remote_hashes       <- return $ getResponseBody res
  let new_hashes      =  difference (fromList (map show local_hashes)) (fromList remote_hashes)
  let deleted_hashes  =  difference (fromList (remote_hashes)) (fromList (map show local_hashes))
  let new_files       =  filter (\(x,y) -> elem (show y) new_hashes) (zip local_files local_hashes)
  let deleted_files   =  filter (\(x,y) -> elem (show y) deleted_hashes) (zip local_files local_hashes)
  new_tracks          <- mapM parse_track (map fst new_files)
  -- mapM_ print new_tracks
  mapM_ print deleted_hashes
  new_media           <- return $ zipWith track_to_media new_files new_tracks
  response            <- post_songs new_media
  print . getResponseStatus $ response
  response            <- delete_songs (toList deleted_hashes)
  print . getResponseStatus $ response


    -- synch playlist func
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
