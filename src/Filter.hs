{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-unused-imports #-}
module Filter where

import Data.Thyme.Clock
import Data.Text
import Data.Aeson
import Data.Thyme.Format.Aeson
import GHC.Generics

import CommandRecord

data Filter = Filter {
    pathContains :: [Text]
 , pathPrefix :: Maybe Text
 , pathSuffix :: Maybe Text
 , pathEqual :: Maybe Text
 , commandContains :: [Text]
 , commandPrefix :: Maybe Text
 , commandSuffix :: Maybe Text
 , commandEqual :: Maybe Text
 , before :: Maybe UTCTime
 , after :: Maybe UTCTime
} deriving (Show, Generic)
 
instance ToJSON Filter
instance FromJSON Filter

exa :: Filter
exa = Filter {
   pathContains = []
 , pathPrefix = Nothing
 , pathSuffix = Nothing
 , pathEqual = Nothing
 , commandContains = ["fld"]
 , commandPrefix = Nothing
 , commandSuffix = Nothing
 , commandEqual = Nothing
 , before = Nothing
 , after = Nothing
}

filterRecords :: Filter -> [CommandRecord] -> [CommandRecord]
filterRecords f r = Prelude.filter (filterRecord f) r

filterRecord :: Filter -> CommandRecord -> Bool
filterRecord Filter{..} CommandRecord{..}  =
  Prelude.all (== True)
  $
  [
      Prelude.all (\pc -> isInfixOf (pc) path) pathContains
    , maybe True (\x -> isPrefixOf x path) pathPrefix
    , maybe True (\x -> isSuffixOf x path) pathSuffix
    , maybe True ((==) path) pathEqual
    -- command
    , Prelude.all (\pc -> isInfixOf (pc) command) commandContains
    , maybe True (\x -> isPrefixOf x command) commandPrefix
    , maybe True (\x -> isSuffixOf x command) commandSuffix
    , maybe True ((==) command) commandEqual
    -- time
    , maybe True ((<) timedate) before
    , maybe True ((>) timedate) after
  ]
