{-# LANGUAGE OverloadedStrings #-}

module Salmon.Data(Media(..),track_to_media,parse_track)where

import           Data.Aeson
import           Salmon.FileSystem
import           Salmon.Server     (url_path)
import           Sound.HTagLib

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

audio_track_getter :: TagGetter AudioTrack
audio_track_getter = AudioTrack <$> titleGetter <*> durationGetter

track_to_media :: String -> AudioTrack -> Media
track_to_media file_name (AudioTrack title duration) =
  Media (drop 6 . show $ title) (unDuration duration) (url_path file_name) -- Drop 6 removes the "Title " label that show places at the front of the string

parse_track :: FilePath -> IO AudioTrack
parse_track = (get_meta_data . get_path)
  where
    get_meta_data = flip getTags $ audio_track_getter
    get_path = (++) dir_path

