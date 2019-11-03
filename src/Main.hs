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

parser :: Parser (IO ())
parser = do
  work
    <$>
      textOption
        (  long "command"
        <> short 'c'
        <> metavar "STRING"
        <> help "command"
        )
    <*>
      textOption
        (  long "path"
        <> short 'p'
        <> metavar "STRING"
        <> help "path"
        )
  <|> printRecords <$> switch ( long "print" <> help "Whether to be quiet" )
  <|> processPendingFiles <$> switch ( long "process-pending" <> help "" )
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
  <|>
  daemon <$> switch ( long "daemon" <> help "Whether to be quiet" )


work :: Text -> Text -> IO ()
work a o = do
  x <- getCurrentTime
  doesFileExist crFile >>= \case
    True -> do
      decodeFileOrFail crFile >>= \case
        Right p -> encodeFile crFile $ CommandRecord a x o : p
        Left e -> error $ show e
    False -> encodeFile crFile $ [CommandRecord a x o]


printFilterRecords :: Bool
  -> [Text] -> Maybe Text -> Maybe Text  -- path
  -> [Text] -> Maybe Text -> Maybe Text  -- command
  -> Maybe Text -> Maybe Text -- before / after
  -> IO ()
printFilterRecords _ a b c d e f g h = do
  let filter' = Filter a b c d e f g h
  print filter'
  decodeFileOrFail crFile >>= \case
    Right p -> do
      pp <- getPendingRecords
      print pp
      let ppp = p ++ pp
      forM_ ((filterRecords filter' ppp) :: [CommandRecord]) (putStrLn . show)
    Left e' -> error $ show e'

printRecords :: Bool -> IO ()
printRecords _ = do
  decodeFileOrFail crFile >>= \case
    Right p -> do
      pp <- getPendingRecords
      print "pending records:"
      forM_ (pp :: [CommandRecord]) (putStrLn . show)
      print "perm records:"
      forM_ (p :: [CommandRecord]) (putStrLn . show)
    Left e -> error $ show e
