{-# LANGUAGE DeriveGeneric #-}
module Message.PrintFilterRecord where

import Filter
import Data.Aeson
import GHC.Generics

data PrintFilterRecord = PrintFilterRecord
  {
    filter :: Filter
  , length :: Int
  , rawJson :: Bool
  } deriving (Show, Generic)

instance ToJSON PrintFilterRecord
instance FromJSON PrintFilterRecord
