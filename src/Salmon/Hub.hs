{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Salmon.Hub(post_songs, get_hashes, delete_songs,login,Credentials(..)) where

import           Data.ByteString.Char8
import           Network.HTTP.Simple
import           Network.HTTP.Types.Header
import           Salmon.Data
import           Data.Aeson

data LoginRequest = LoginRequest { username :: String , password :: String } deriving (Show)

instance ToJSON LoginRequest where
  toJSON (LoginRequest username password) =
    object
      [ "username" .= username
      , "password" .= password
      ]


newtype Credentials = Credentials String deriving Show

  -- TODO These functions should just return a Maybe, they shoudld extract the response body and handle failure cases themselves
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

login :: String -> String -> IO Credentials
login username password = do
  res :: Response ByteString <- httpBS $ setRequestBodyJSON (LoginRequest username password) "POST http://127.0.0.1:8080/api/login"
  let id = getResponseBody res
  return $ Credentials (read . unpack $ id)
