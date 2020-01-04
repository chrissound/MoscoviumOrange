{-# LANGUAGE RecordWildCards #-}
module Filter where


import Data.Thyme.Clock
import Data.Text

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
} deriving Show

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
  ]
  <>
  [
      Prelude.all (\pc -> isInfixOf (pc) command) commandContains
    , maybe True (\x -> isPrefixOf x command) commandPrefix
    , maybe True (\x -> isSuffixOf x command) commandSuffix
    , maybe True ((==) command) commandEqual
  ]
