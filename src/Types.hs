{-# LANGUAGE DeriveGeneric #-}
module Types where

import Control.Monad.State
import CommandRecord
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

data MoscoOperation = MoscoDaemon | MoscoPrinter

data MoscConfig = MoscConfig {
    datadir :: FilePath
  , config :: FilePath
  , pendingPath :: FilePath
  , socket :: FilePath
  } deriving Show

data MoscState = MoscState
  {
    moscConf :: MoscConfig
  , crecords :: [CommandRecord]
  } deriving Show

type Mosco = StateT MoscState IO

data Message = Message {
  command :: String,
  path :: String } deriving Generic

instance ToJSON Message
instance FromJSON Message
