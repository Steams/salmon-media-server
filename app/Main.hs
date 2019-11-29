{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Semigroup      ((<>))
import           Options.Applicative
import           Salmon (run,Config(..))

args :: Parser Salmon.Config
args = Salmon.Config
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
      <*> strOption
          ( long "folder"
         <> short 'f'
         <> metavar "FOLDER"
         <> help "Path to folder where media library is stored" )

main :: IO ()
main =
  let
    opts = info (args <**> helper) ( fullDesc <> progDesc "Salmon Media server HLS streaming process" )
  in
    do
      config <- execParser opts
      Salmon.run config
