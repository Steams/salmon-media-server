{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Semigroup      ((<>))
import           Options.Applicative
import           Salmon

data Args =
  Args
    { username :: String
    , password :: String
    }

args :: Parser Args
args = Args
      <$> strOption
          ( long "username"
         <> short 'u'
         <> metavar "USERNAME"
         <> help "User account name for Salmon" )
      <*> strOption
          ( long "password"
         <> short 'p'
         <> metavar "PASSWORD"
         <> help "User password name for Salmon" )

main :: IO ()
main =
  let
    opts = info (args <**> helper)
      ( fullDesc <> progDesc "Media server" )
  in
    do
      options <- execParser opts
      Salmon.run (username options) (password options)
