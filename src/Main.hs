{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS -Wno-unused-imports #-}

module Main where

import Options.Applicative
import Options.Applicative.Text
import Control.Monad (join)
import Data.Monoid ((<>))
import Data.Text
import Data.Thyme.Clock
import Data.Binary
import Data.Binary.Orphans
import GHC.Generics
import Data.Thyme.Internal.Micro
import Data.Thyme.Format
import System.Directory
import Control.Monad
import System.IO
import Data.List.Split
import Data.String.Conversions
import Debug.Trace
import Text.Pretty.Simple (pPrint)
-- import Data.Time.Format (defaultTimeLocale)
import System.Locale

import MoscoviumOrangePrelude
import CommandRecord
import Filter
import Daemon
import Printer

main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  -- <> header ""
  -- <> progDesc "What does this thing do?"
  )

limiter :: Parser Int
limiter =
       option auto (
         (  long "limit"
         <> metavar "NUMBER"
         <> help "limit"
         <> value 30
         )
       )

autotime :: ReadM UTCTime
autotime = eitherReader (\s -> case (parseTime defaultTimeLocale timeFormatThingy s) of
                            Just x -> Right x; Nothing -> Left "Invalid timestamp"
                        )

parser :: Parser (IO ())
parser = do
  printRecords
     <$> switch ( long "print" <> help "Print records" )
     <*> limiter
  <|>
  printFilterRecords
    <$> switch ( long "print-filter" <> help "Print records with filter" )
    <*> many (
      textOption
        (  long "path-contains"
        <> metavar "STRING"
        )
      )
    <*>
      optional (
      textOption
        (  long "path-prefix"
        <> metavar "STRING"
        )
      )
    <*>
      optional (
      textOption
        (  long "path-suffix"
        <> metavar "STRING"
        )
      )
    <*>
      optional (
      textOption
        (  long "path"
        <> metavar "STRING"
        <> help "path equals"
        )
      )
    <*> many (
      textOption
        (  long "command-contains"
        <> metavar "STRING"
        )
      )
    <*>
      optional (
      textOption
        (  long "command-prefix"
        <> metavar "STRING"
        )
      )
    <*>
      optional (
      textOption
        (  long "command-suffix"
        <> metavar "STRING"
        )
      )
    <*>
      optional (
      textOption
        (  long "command"
        <> metavar "STRING"
        <> help "command equals"
        )
      )
    <*>
      optional (
      option autotime
        (  long "before"
        <> metavar "STRING"
        <> help "filter records that occurred before time"
        )
      )
    <*>
      optional (
      option autotime
        (  long "after"
        <> metavar "STRING"
        <> help "filter records that occurred after time"
        )
      )
    <*> limiter
  <|>
  daemon <$> switch ( long "daemon" <> help "Run daemon listener" )

printFilterRecords :: Bool
  -> [Text] -> Maybe Text -> Maybe Text -> Maybe Text -- path
  -> [Text] -> Maybe Text -> Maybe Text -> Maybe Text -- command
  -> Maybe UTCTime -> Maybe UTCTime -- before / after
  -> Int
  -> IO ()
printFilterRecords _ pa pb pc pd ca cb cc cd tb ta l = do
  let filter' = Filter {
    pathContains     =      pa
  , pathPrefix       =      pb
  , pathSuffix       =      pc
  , pathEqual        =      pd
  , commandContains  =      ca
  , commandPrefix    =      cb
  , commandSuffix    =      cc
  , commandEqual     =      cd
  , before           =      tb
  , after            =      ta
  }
  print filter'
  crFile >>= decodeFileOrFail >>= \case
    Right p -> do
      pp <- getPendingRecords
      printRecords' (filterRecords filter' (p ++ pp)) l
    Left e' -> error $ show e'
