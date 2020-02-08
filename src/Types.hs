module Types where

import Control.Monad.State

data MoscConfig = MoscConfig {
    datadir :: FilePath
  , config :: FilePath
  , pendingPath :: FilePath
  , socket :: FilePath
  }

type Mosco = StateT MoscConfig IO
