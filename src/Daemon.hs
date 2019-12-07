{-# OPTIONS -Wno-unused-imports #-}
{-# OPTIONS -Wno-unused-matches #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
module Daemon where

import CommandRecord
import MoscoviumOrangePrelude

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
import System.Directory
import Data.Functor

import Network.Socket
import Network.Socket.ByteString as NBS
import qualified Data.ByteString.Char8 as C

import Data.Aeson (decode, Object)
import Data.Aeson (FromJSON, ToJSON)
import Data.HashMap.Strict (keys, (!))
import Control.Arrow

import GHC.Generics

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

talk :: TVar [CommandRecord] ->  String -> Socket -> IO ()
talk pending s conn = do
  msg <- NBS.recv conn 4096
  case C.null msg of
    False -> do
      talk pending (s ++ cs msg) conn
    True -> do
      ct <- getCurrentTime
      case ((decode $ cs s :: Maybe Message)) of
        Just x -> do
          atomically $ modifyTVar' pending
            (
              (:) (CommandRecord (cs $ Daemon.command x) ct (cs $ Daemon.path x))
            )
        _ -> do
          putStrLn "Failed to decode message:"
          putStrLn s
      print s
      print "Final message"
      close conn

daemon :: Bool -> IO ()
daemon _ = do
  socketFile >>= doesFileExist >>= bool
    (pure ())
    (socketFile >>= removeFile)
  print "Running daemon"
  soc <- socket AF_UNIX Stream 0
  socketFile >>= (bind soc . SockAddrUnix)
  listen soc maxListenQueue
  processPendingFiles
  x <- newTVarIO []
  i <- newTMVarIO (0 :: Int)
  _ <- forkIO $ savePendingRecordsToFs x i
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

getPendingRecords :: IO [CommandRecord]
getPendingRecords = do
  p <- crDirPending >>= getDirectoryContents
  let filtered = sort $ map read $ filter (all isDigit) p :: [Int]
  (forM filtered $ \i -> (crDirPending) <&> (++ show i) >>= decodeFile) >>= return . join

appendRecords :: [CommandRecord] -> IO ()
appendRecords cr = do
  putStrLn "Appending pending records"
  crFile >>= doesFileExist >>= \case
    True -> do
       crFile >>= decodeFileOrFail >>= \case
        Right p -> crFile >>= (\x -> encodeFile x $ p ++ cr)
        Left e -> error $ show e
    False -> crFile >>= (\x -> encodeFile x $ cr)

fileExist :: String -> IO (Either String ())
fileExist f = doesFileExist f >>= return . bool (Left $ "File does not exist: " ++ f) (Right ())

getConfigDir :: IO FilePath
getConfigDir = getXdgDirectory XdgConfig "moscoviumOrange"

crFile :: IO FilePath
crFile = (getConfigDir <&> (++ "/commandRecord.data"))

crDirPending :: IO FilePath
crDirPending = (getConfigDir <&> (++ "/pending/"))

socketFile :: IO FilePath
socketFile = (getConfigDir <&> (++ "/monitor.soc"))
