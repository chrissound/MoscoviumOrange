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

import MoscoviumOrangePrelude
import CommandRecord
import Filter
import Daemon
import Printer

main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  <> header "General program title/description"
  <> progDesc "What does this thing do?"
  )

target :: Parser String
target = strOption
  (  long "hello"
  <> metavar "TARGET"
  <> help "Target for the greeting" )

limiter :: Parser Int
limiter =
       option auto (
         (  long "limit"
         <> metavar "NUMBER"
         <> help "limit"
         <> value 30
         )
       )

parser :: Parser (IO ())
parser = do
  printRecords
     <$> switch ( long "print" <> help "Whether to be quiet" )
     <*> limiter
  <|>
  printFilterRecords
    <$> switch ( long "print-filter" <> help "Print with filter" )
    <*> many (
      textOption
        (  long "path-contains"
        <> metavar "STRING"
        <> help "command"
        )
      )
    <*>
      optional (
      textOption
        (  long "path-prefix"
        <> metavar "STRING"
        <> help "command"
        )
      )
    <*>
      optional (
      textOption
        (  long "path-suffix"
        <> metavar "STRING"
        <> help "command"
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
        <> help "command"
        )
      )
    <*>
      optional (
      textOption
        (  long "command-prefix"
        <> metavar "STRING"
        <> help "command"
        )
      )
    <*>
      optional (
      textOption
        (  long "command-suffix"
        <> metavar "STRING"
        <> help "command"
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
      textOption
        (  long "before"
        <> metavar "STRING"
        <> help "command"
        )
      )
    <*>
      optional (
      textOption
        (  long "after"
        <> metavar "STRING"
        <> help "command"
        )
      )
    <*> limiter
  <|>
  daemon <$> switch ( long "daemon" <> help "Run daemon listener" )

printFilterRecords :: Bool
  -> [Text] -> Maybe Text -> Maybe Text -> Maybe Text -- path
  -> [Text] -> Maybe Text -> Maybe Text -> Maybe Text -- command
  -> Maybe Text -> Maybe Text -- before / after
  -> Int
  -> IO ()
printFilterRecords _ pa pb pc pd ca cb cc cd ta tb l = do
  let filter' = Filter pa pb pc pd ca cb cc cd ta tb
  print filter'
  decodeFileOrFail crFile >>= \case
    Right p -> do
      pp <- getPendingRecords
      printRecords' (filterRecords filter' (p ++ pp)) l
    Left e' -> error $ show e'
