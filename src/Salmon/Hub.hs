{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}

module Salmon.Hub(post_songs, get_hashes, delete_songs,login,Credentials(..)) where

import           Data.ByteString.Char8
import           Network.HTTP.Simple
import           Network.HTTP.Types.Header
import           Salmon.Data
import           Data.Aeson

base_url = "http://127.0.0.1:8080/"

data Endpoint
  = MEDIA
  | GET_HASHES
  | DELETE
  | LOGIN

to_url :: Endpoint -> Request
to_url =
  \case
    MEDIA      -> parseRequest_ $ "POST " ++ base_url ++ "media"
    GET_HASHES -> parseRequest_ $ "GET "  ++ base_url ++ "synch"
    DELETE     -> parseRequest_ $ "POST " ++ base_url ++ "synch"
    LOGIN      -> parseRequest_ $ "POST " ++ base_url ++ "api/login"

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
  httpJSONEither
  $ addRequestHeader hAuthorization (pack user_id)
  $ setRequestBodyJSON songs
  $ to_url MEDIA

get_hashes :: Credentials -> IO (Response [String])
get_hashes (Credentials user_id) =
  httpJSON
  $ addRequestHeader hAuthorization (pack user_id)
  $ to_url GET_HASHES

delete_songs :: Credentials -> [String] -> IO (Response (Either JSONException ()))
delete_songs (Credentials user_id) hashes =
  httpJSONEither
  $ addRequestHeader hAuthorization (pack user_id)
  $ setRequestBodyJSON hashes
  $ to_url DELETE

login :: String -> String -> IO Credentials
login username password = do
  res :: Response ByteString <- httpBS $ setRequestBodyJSON (LoginRequest username password) $ to_url LOGIN
  let id = getResponseBody res
  return $ Credentials (read . unpack $ id)
