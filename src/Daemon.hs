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

import Network.Socket
import Network.Socket.ByteString as NBS
import qualified Data.ByteString.Char8 as C

import Data.Aeson (decode, Object)
import Data.Aeson (FromJSON, ToJSON)
import Data.HashMap.Strict (keys, (!))

import GHC.Generics

data Message = Message {
  command :: String,
  path :: String } deriving Generic

instance ToJSON Message
instance FromJSON Message

daemonLoop :: Handle -> TVar [CommandRecord] -> String -> IO ()
daemonLoop crFifoH pending s = do
  print "wtf"
  -- hIsEOF crFifoH >>= \case
  --   False -> do
  --     cr <- (s ++) <$> hGetLine crFifoH
  --     print $ show cr
  --     let input = Data.List.Split.splitOn ([toEnum 0]::String) (cr :: String)
  --     case input of
  --       (c:p:[]) -> do
  --         print "Successfully added record"
  --         putStrLn ""
  --         putStrLn ""
  --         x <- getCurrentTime
  --         atomically $ modifyTVar' pending (\records -> CommandRecord (cs c) x (cs p) : records)
  --         daemonLoop crFifoH pending ""
  --       _  -> do
  --         putStrLn $ (traceShow "Input^^^") (input !! 0)
  --         putStrLn $ (traceShow "Input^^^") (input !! 1)
  --         putStrLn $ (traceShow "cr^^^") cr
  --         print "Not sufficient input"
  --         putStrLn ""
  --         putStrLn ""
  --         daemonLoop crFifoH pending (cr)
  --   True -> do
  --     threadDelay (round $ 0.1 * (10^6))
  --     daemonLoop crFifoH (pending) s

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
        print "Saving records to FS"
        i'' <- atomically $ do
          i' <- takeTMVar i
          putTMVar i (i' + 1)
          return i'
        let pfp = crDirPending ++ (show i'')
        fileExist pfp >>= \ case
          Right () -> error "file already exists!"
          Left _ -> encodeFile pfp newEntries

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
        _ -> print "fucked up"
      print s
      print "Final message"
      close conn

daemon :: Bool -> IO ()
daemon _ = do
  let spath = "/home/chris/.config/moscoviumOrange/monitor.soc"
  doesFileExist spath >>= bool
    (pure ())
    (removeFile spath)
  print "Running daemon"
  soc <- socket AF_UNIX Stream 0
  bind soc $ SockAddrUnix spath
  listen soc maxListenQueue
  processPendingFiles
  x <- newTVarIO []
  i <- newTMVarIO (0 :: Int)
  _ <- forkIO $ savePendingRecordsToFs x i
  talkLoop x soc
  -- print "done????"
  -- x <- NBS.send soc (cs "\n")
  -- print x
  -- sClose soc
  -- processPendingFiles True
  -- fileExist crFile >>= \case
  --   Right _ -> return ()
  --   Left _ -> do
  --     putStrLn "Creating new data file"
  --     encodeFile crFile ([] :: [CommandRecord])
  -- crFifoH <- openFile crFifo ReadMode
  -- x <- newTVarIO []
  -- i <- newTMVarIO (0 :: Int)
  -- daemonLoop crFifoH x ""

processPendingFiles :: IO ()
processPendingFiles = do
  -- This could fuck up if it gets executed by two processes
  getPendingRecords >>= appendRecords
  deletePendingRecords

deletePendingRecords :: IO ()
deletePendingRecords = do
  putStrLn "Deleting pending records"
  p <- getDirectoryContents crDirPending
  let filtered = sort $ map read $ filter (all isDigit) p :: [Int]
  putStrLn $ "Found (" ++ show filtered ++ ") pending files"
  forM_ filtered $ \i -> do
    let pfp = crDirPending ++ show i
    removeFile pfp

getPendingRecords :: IO [CommandRecord]
getPendingRecords = do
  p <- getDirectoryContents crDirPending
  let filtered = sort $ map read $ filter (all isDigit) p :: [Int]
  (forM filtered $ \i -> do
    let pfp = crDirPending ++ show i
    x <- decodeFile pfp
    return x
    ) >>= return . join

appendRecords :: [CommandRecord] -> IO ()
appendRecords cr = do
  putStrLn "Appending pending records"
  doesFileExist crFile >>= \case
    True -> do
      decodeFileOrFail crFile >>= \case
        Right p -> encodeFile crFile $ p ++ cr
        Left e -> error $ show e
    False -> encodeFile crFile $ cr

fileExist :: String -> IO (Either String ())
fileExist f = doesFileExist f >>= return . bool (Left $ "File does not exist: " ++ f) (Right ())

crFile :: FilePath
crFile = "/home/chris/.config/moscoviumOrange/commandRecord.data"

crDirPending :: FilePath
crDirPending = "/home/chris/.config/moscoviumOrange/pending/"

-- crFifo :: FilePath
-- crFifo = "/home/chris/.config/moscoviumOrange/commandRecord.fifo"
-- --crFifo = "test.fifo"

