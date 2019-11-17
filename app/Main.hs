{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
import           Data.List
import           Network.HTTP.Simple
import           Sound.HTagLib
import           System.Directory
import           System.Process
import           Text.Printf
import           Web.Scotty


       -- Constants

dir_path :: [Char]
dir_path = "/home/steams/Development/audigo/salmon-media-server/resources/"

working_dir_path :: [Char]
working_dir_path = "/home/steams/Development/audigo/salmon-media-server/resources/.salmon/"

playlist_path :: [Char] -> [Char]
playlist_path mp3 = working_dir_path ++ (take (length mp3 - 4) mp3) ++ ".m3u8"

url_path :: [Char] -> [Char]
url_path mp3 = "localhost:3000/" ++ (take (length mp3 - 4) mp3) ++ ".m3u8"

       -- Data

data Media = Media { title :: String , duration :: Int, path :: String} deriving (Show)

instance ToJSON Media where
  toJSON (Media title duration playlist) =
    object ["title" .= title, "duration" .= duration, "playlist" .= playlist]

data AudioTrack =
  AudioTrack
    { atTitle    :: Title
    , atDuration :: Duration
    }
  deriving (Show)

audioTrackGetter :: TagGetter AudioTrack
audioTrackGetter = AudioTrack <$> titleGetter <*> durationGetter

track_to_media :: String -> AudioTrack -> Media
track_to_media file_name (AudioTrack title duration)=
  Media (show title) (unDuration duration) (url_path file_name)

ffmpeg_generate :: PrintfType t => [Char] -> t
ffmpeg_generate mp3 =
  printf "ffmpeg -i %s -c:a aac -b:a 64k -vn -hls_list_size 0 %s" mp3_path playlist
  where
    playlist = playlist_path mp3
    mp3_path = dir_path ++ mp3

       -- IO

get_files :: IO [String]
get_files = filter (isSuffixOf ".mp3") <$> getDirectoryContents dir_path

create_working_dir :: IO ()
create_working_dir = do
  bool <- doesDirectoryExist working_dir_path
  case bool of
    True  -> return ()
    False -> createDirectory working_dir_path

generate_playlist :: String -> IO ()
generate_playlist mp3 = do
  bool <- doesFileExist $ playlist_path mp3
  case bool of
    True  -> return ()
    False -> callCommand $ ffmpeg_generate mp3


serve_files :: ScottyM ()
serve_files =
  get "/:file" $ do
    file_name <- param "file"
    file $ working_dir_path ++ file_name

run_server :: IO ()
run_server = scotty 3000 $ do serve_files

executeRequest :: Request -> IO (Response (Either JSONException ()))
executeRequest = httpJSONEither

initialize_playlists :: IO ()
initialize_playlists = do
  files <- get_files
  create_working_dir
  mapM_ generate_playlist files

synch_with_hub :: IO ()
synch_with_hub =
  let
    request body  = setRequestBodyJSON body "POST http://localhost:8080/media"
    get_meta_data = flip getTags $ audioTrackGetter
    get_path      = (++) dir_path
  in do
    files     <- get_files
    tracks    <- mapM (get_meta_data . get_path) files
    mapM_ print tracks
    let media = zipWith track_to_media files tracks
    let reqs  = map request media
    responses <- mapM executeRequest reqs
    mapM_ (print . getResponseStatus) responses

main :: IO ()
main = do
  initialize_playlists
  synch_with_hub
  putStrLn "settup complete"
  run_server
