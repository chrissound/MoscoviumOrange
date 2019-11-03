{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS -Wno-unused-imports #-}
module CommandRecord where

import Data.Text
import Data.Thyme.Clock
import Data.Thyme.Format
import GHC.Generics

import System.Locale
import Data.Binary
import ThymeBinaryInstances


data CommandRecord = CommandRecord {
    command :: Text
  , timedate :: UTCTime
  , path :: Text
  } deriving (Generic)

instance Binary CommandRecord

instance Show CommandRecord
  where show cr = show (command cr) ++ " " ++ formatTime defaultTimeLocale "%d/%m/%Y %T %q" (timedate cr) ++ " " ++ show (path cr)
