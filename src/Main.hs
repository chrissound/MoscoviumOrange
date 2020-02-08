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
import Control.Monad.State
import Data.Maybe
import DataStore

import MoscoviumOrangePrelude
import CommandRecord
import Filter
import Daemon
import Printer
import Types

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

rawJsonFlag :: Parser Bool
rawJsonFlag = switch ( long "json" <> help "output as json (should be much faster)" )

autotime :: ReadM UTCTime
autotime = eitherReader (\s -> case (parseTime defaultTimeLocale timeFormatThingy s) of
                            Just x -> Right x; Nothing -> Left "Invalid timestamp"
                        )

rr :: Parser (Mosco ())
rr = pure $ pure ()

moscparams ::
  Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Mosco a -> IO a
moscparams dd c pp sp x = do
  ddd <- crFile
  cd <- pure "not used"
  ppd <- crDirPending
  spd <- socketFile
  evalStateT x
    $ MoscConfig
      (maybe ddd cs dd)
      (maybe cd cs c)
      (maybe ppd cs pp)
      (maybe spd cs sp)

moscF' :: Text -> Text -> Mosco ()
moscF' x x' = do
  liftIO $ print x
  liftIO $ print x'
  liftIO $ print "testabcxyz"

moscparams' :: Parser (Mosco ()) -> Parser (IO ())
moscparams' x = moscparams
  <$> optional (textOption ( long "data-path" <> help "main data file" ))
  <*> optional (textOption ( long "config-path" ))
  <*> optional (textOption ( long "pending-path" ))
  <*> optional (textOption ( long "socket-path" ))
  <*> x

parser :: Parser (IO ())
parser = do
  printRecords
     <$> switch ( long "print" <> help "Print records" )
     <*> limiter
     <*> rawJsonFlag
  <|>
  moscparams' (
    printFilterRecordsParser
    <|>
      const daemon
      <$> switch (long "daemon" <> help "Run daemon listener" )
    )

printFilterRecordsParser :: Parser (Mosco ())
printFilterRecordsParser =
  printFilterRecords
        <$> many (
          textOption
            (  long "path-contains"
            <> metavar "STRING"
            <> help "filter path contains"
            )
          )
        <*>
          optional (
          textOption
            (  long "path-prefix"
            <> metavar "STRING"
            <> help "filter path prefix"
            )
          )
        <*>
          optional (
          textOption
            (  long "path-suffix"
            <> metavar "STRING"
            <> help "filter path suffix"
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
            <> help "filter command contains"
            )
          )
        <*>
          optional (
          textOption
            (  long "command-prefix"
            <> metavar "STRING"
            <> help "filter command prefix"
            )
          )
        <*>
          optional (
          textOption
            (  long "command-suffix"
            <> metavar "STRING"
            <> help "filter command equals"
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
            <> metavar "timestamp"
            <> help "filter records that occurred before time"
            )
          )
        <*>
          optional (
          option autotime
            (  long "after"
            <> metavar "timestamp"
            <> help "filter records that occurred after time"
            )
          )
        <*> limiter
        <*> rawJsonFlag

printFilterRecords :: 
     [Text] -> Maybe Text -> Maybe Text -> Maybe Text -- path
  -> [Text] -> Maybe Text -> Maybe Text -> Maybe Text -- command
  -> Maybe UTCTime -> Maybe UTCTime -- before / after
  -> Int
  -> Bool
  -> Mosco ()
printFilterRecords pa pb pc pd ca cb cc cd tb ta l rj = do
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
  liftIO $ do
    r <- getAllRecords
    printRecords' (filterRecords filter' r) l rj
