{-# LANGUAGE OverloadedStrings #-}

module Salmon.Data(Media(..),track_to_media,parse_track, AudioTrack(..))where

import           Data.Aeson
import           Data.Digest.Pure.MD5
import           Path
import           Salmon.FileSystem
import           Salmon.Server        (url_path,art_url)
import           Sound.HTagLib

data Media =
  Media
    { title :: String
    , artist :: String
    , album :: String
    , duration :: Int
    , number :: Maybe Int
    , path :: String
    , art :: String
    , hash :: String
    }
  deriving (Show)

instance ToJSON Media where
  toJSON (Media title artist album duration number playlist art hash) =
    object
      [ "title" .= title
      , "artist" .= artist
      , "album" .= album
      , "duration" .= duration
      , "number" .= number
      , "playlist" .= playlist
      , "art" .= art
      , "hash" .= hash
      ]

data AudioTrack =
  AudioTrack
    { atTitle    :: Title
    , atDuration :: Duration
    , atArtist   :: Artist
    , atAlbum    :: Album
    , atTrack    :: Maybe TrackNumber
    }
  deriving (Show)

audio_track_getter :: TagGetter AudioTrack
audio_track_getter = AudioTrack <$> titleGetter <*> durationGetter <*> artistGetter <*> albumGetter <*> trackNumberGetter

-- TODO USE NEW TYPES TO PREVENT SENDING NAME AND HASH IN WRONG ORDER
track_to_media :: (Path Abs File,String) -> AudioTrack -> Media
track_to_media (file, file_hash) (AudioTrack title duration artist album track_number) =
  Media
    (read . drop 6 . show $ title) -- Drop 6 removes the "Title " label that show places at the front of the string
    (read . drop 6 . show $ artist)
    (read . drop 6 . show $ album)
    (unDuration duration)
    (unTrackNumber <$> track_number)
    -- TODO path should be based on hash not name incase of name clash between albums
    (url_path file_hash)
    -- TODO This is silly fix later
    (art_url  (read . drop 6 . show $ album))
    (file_hash)

parse_track :: Path Abs File -> IO AudioTrack
parse_track path = get_meta_data $ fromAbsFile path
  where
    get_meta_data = flip getTags $ audio_track_getter

