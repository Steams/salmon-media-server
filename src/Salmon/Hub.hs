{-# LANGUAGE OverloadedStrings #-}

module Salmon.Hub(post_song) where

import           Network.HTTP.Simple
import           Salmon.Data

post_song :: Media -> IO (Response (Either JSONException ()))
post_song song =
  httpJSONEither $ setRequestBodyJSON song "POST http://127.0.0.1:8080/media"

