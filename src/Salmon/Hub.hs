{-# LANGUAGE OverloadedStrings #-}

module Salmon.Hub(post_songs, get_hashes, delete_songs, Credentials(..)) where

import           Data.ByteString.Char8
import           Network.HTTP.Simple
import           Network.HTTP.Types.Header
import           Salmon.Data




newtype Credentials = Credentials String

post_songs :: Credentials -> [Media] -> IO (Response (Either JSONException ()))
post_songs (Credentials user_id) songs =
  httpJSONEither $
  addRequestHeader hAuthorization (pack user_id) $
  setRequestBodyJSON songs "POST http://127.0.0.1:8080/media"

get_hashes :: Credentials -> IO (Response [String])
get_hashes (Credentials user_id) =
  httpJSON $
  addRequestHeader hAuthorization (pack user_id) $
  "http://127.0.0.1:8080/synch"

delete_songs :: Credentials -> [String] -> IO (Response (Either JSONException ()))
delete_songs (Credentials user_id) hashes =
  httpJSONEither $
  addRequestHeader hAuthorization (pack user_id) $
  setRequestBodyJSON hashes "POST http://127.0.0.1:8080/synch"
