{-# LANGUAGE OverloadedStrings #-}

module Salmon.Data(Media(..),track_to_media,parse_track)where

import           Data.Aeson
import           Data.Digest.Pure.MD5
import           Path
import           Salmon.FileSystem
import           Salmon.Server        (url_path)
import           Sound.HTagLib

data Media = Media { title :: String , duration :: Int, path :: String, hash :: String} deriving (Show)

instance ToJSON Media where
  toJSON (Media title duration playlist hash) =
    object ["title" .= title, "duration" .= duration, "playlist" .= playlist, "hash" .= hash]

data AudioTrack =
  AudioTrack
    { atTitle    :: Title
    , atDuration :: Duration
    }
  deriving (Show)

audio_track_getter :: TagGetter AudioTrack
audio_track_getter = AudioTrack <$> titleGetter <*> durationGetter

                     -- TODO USE NEW TYPES TO PREVENT SENDING NAME AND HASH IN WRONG ORDER
track_to_media :: (Path Abs File,String) -> AudioTrack -> Media
track_to_media (file, file_hash) (AudioTrack title duration) =
  Media
    (drop 6 . show $ title) -- Drop 6 removes the "Title " label that show places at the front of the string
    (unDuration duration)
    (url_path $ get_file_name file)
    (file_hash)

parse_track :: Path Abs File -> IO AudioTrack
parse_track path = get_meta_data $ fromAbsFile path
  where
    get_meta_data = flip getTags $ audio_track_getter

