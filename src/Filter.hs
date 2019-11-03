{-# LANGUAGE RecordWildCards #-}
module Filter where

import Data.Text

import CommandRecord

data Filter = Filter {
    pathContains :: [Text]
 , pathPrefix :: Maybe Text
 , pathSuffix :: Maybe Text
 , commandContains :: [Text]
 , commandPrefix :: Maybe Text
 , commandSuffix :: Maybe Text
 , before :: Maybe Text
 , after :: Maybe Text
} deriving Show

filterRecords :: Filter -> [CommandRecord] -> [CommandRecord]
filterRecords f r = Prelude.filter (filterRecord f) r

filterRecord :: Filter -> CommandRecord -> Bool
filterRecord Filter{..} CommandRecord{..}  =
  Prelude.all (== True)
  [
      Prelude.all (\pc -> isInfixOf (pc) path) pathContains
    , maybe True (\x -> isPrefixOf x path) pathPrefix
    , maybe True (\x -> isSuffixOf x path) pathSuffix
    , Prelude.all (\pc -> isInfixOf (pc) command) commandContains
    , maybe True (\x -> isPrefixOf x command) commandPrefix
    , maybe True (\x -> isSuffixOf x command) commandSuffix
  ]
