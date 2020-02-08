{-# OPTIONS -Wno-unused-imports #-}
{-# OPTIONS -Wno-unused-matches #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
module Daemon where

import Printer
import DataStore
import CommandRecord
import MoscoviumOrangePrelude
import Data.Thyme.Internal.Micro
import Data.Thyme.Clock
import Filter
import qualified Data.Aeson as DAJ

import Control.Applicative
import Data.List
import Data.Foldable
import Data.Thyme.Clock ( getCurrentTime )
import Data.Binary ( decodeFileOrFail, encode, encodeFile, decodeFile)
import Data.ByteString (appendFile)
import Data.Bool (bool)
import System.Directory ( doesFileExist )
import System.IO
    ( Handle, IOMode(ReadMode), hGetLine, openFile, hIsEOF )
import Data.List.Split ( splitOn )
import Data.String.Conversions ( cs )
import Debug.Trace ( traceShow )
import Control.Concurrent.STM
--import Control.Concurrent.STM.TMVar
import Control.Concurrent
-- import Control.Concurrent.STM.TVar
import Data.Char (isDigit)
import System.Directory
import Data.List (sort)
import Control.Monad
import Control.Concurrent
import Control.Monad
import System.IO
import System.Directory
import Data.Functor

import Network.Socket
import Network.Socket.ByteString as NBS
import qualified Data.ByteString.Char8 as C

import Data.Aeson (decode, Object)
import Data.Aeson (FromJSON, ToJSON)
import Data.HashMap.Strict (keys, (!))
import Control.Arrow
import Types (Mosco)
import qualified Types as MoscoTypes
import Control.Monad.IO.Class

import GHC.Generics
import Control.Monad.State

data Message = Message {
  command :: String,
  path :: String } deriving Generic

instance ToJSON Message
instance FromJSON Message

savePendingRecordsToFs :: TVar [CommandRecord] -> TMVar Int -> IO ()
savePendingRecordsToFs x i = do
  let seconds = 1
  threadDelay (seconds * 10^6)
  runPeriodicallyBigDrift (fromIntegral seconds) $ do
    newEntries <- atomically $ swapTVar x []
    case newEntries of
      [] -> do
        return ()
      _ -> do
        i'' <- atomically $ do
          i' <- takeTMVar i
          putTMVar i (i' + 1)
          return i'
        (crDirPending) <&> (++ show i'') >>= fileExist >>= \ case
          Right () -> error "Warning! File already exists! Failed to save new entries."
          Left _ -> (crDirPending) <&> (++ show i'') >>= (\z -> encodeFile z newEntries)

talkLoop :: TVar [CommandRecord] -> Socket -> IO b
talkLoop p sock = do
  (conn,_) <- accept sock
  talk p "" conn
  talkLoop p sock

posFilter :: String -> Socket -> Maybe (IO ())
posFilter s conn =
  case (decode $ cs s :: Maybe Filter) of
    Just x -> pure $ do
      r <- getAllRecords
      output <- (printRecords'' $ filterRecords exa r ) 10 False
      sendAll conn output
    Nothing -> Nothing

posMsg :: [Char]
                -> TVar [CommandRecord]
                -> UTCTime
                -> Maybe (IO ())
posMsg s pending ct =
  case (decode $ cs s :: Maybe Message) of
    Just x -> pure $ do
      atomically $ modifyTVar' pending
                (
                  (:) (CommandRecord (cs $ Daemon.command x) ct (cs $ Daemon.path x))
                )
    _ -> Nothing

talk :: TVar [CommandRecord] ->  String -> Socket -> IO ()
talk pending s conn = do
  msg <- NBS.recv conn 4096
  case C.null msg of
    False -> talk pending (s ++ cs msg) conn
    True -> do
      ct <- getCurrentTime
      case asum [
          posMsg s pending ct
        , posFilter s conn
        ] of
        Just x -> x
        Nothing -> do
          putStrLn $ "Found invalid daemon message:"
          putStrLn s
      close conn

daemon :: Mosco ()
daemon = do
  mc <- get
  liftIO $ do
    doesFileExist (MoscoTypes.socket mc) >>= bool
      (pure ())
      (do
          print "Removing file..."
          removeFile $ MoscoTypes.socket mc)
    print "Running daemon"
    soc <- socket AF_UNIX Stream 0
    bind soc . SockAddrUnix . MoscoTypes.socket $ mc
    listen soc maxListenQueue
    -- processPendingFiles
    x <- newTVarIO []
    i <- newTMVarIO (0 :: Int)
    -- _ <- forkIO $ savePendingRecordsToFs x i
    talkLoop x soc

processPendingFiles :: IO ()
processPendingFiles = do
  -- This could fuck up if it gets executed by two processes
  getPendingRecords >>= appendRecords
  deletePendingRecords

deletePendingRecords :: IO ()
deletePendingRecords = do
  putStrLn "Deleting pending records"
  p <- crDirPending >>= getDirectoryContents
  let filtered = sort $ map read $ filter (all isDigit) p :: [Int]
  putStrLn $ "Found (" ++ show filtered ++ ") pending files"
  forM_ filtered $ \i -> (crDirPending) <&> (++ show i) >>= removeFile

