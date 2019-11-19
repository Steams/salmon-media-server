{-# LANGUAGE OverloadedStrings #-}

module Salmon.Hub(post_songs, get_hashes, delete_songs) where

import           Network.HTTP.Simple
import           Salmon.Data

post_songs :: [Media] -> IO (Response (Either JSONException ()))
post_songs songs =
  httpJSONEither $ setRequestBodyJSON songs "POST http://127.0.0.1:8080/media"

get_hashes :: IO (Response [String])
get_hashes = httpJSON "http://127.0.0.1:8080/synch"

delete_songs :: [String] -> IO (Response (Either JSONException ()))
delete_songs hashes =
  httpJSONEither $ setRequestBodyJSON hashes "POST http://127.0.0.1:8080/synch"
