{-# OPTIONS -Wno-unused-imports #-}
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

daemonLoop :: Handle -> TVar [CommandRecord] -> String -> IO ()
daemonLoop crFifoH pending s = do
  hIsEOF crFifoH >>= \case
    False -> do
      cr <- (s ++) <$> hGetLine crFifoH
      print $ show cr
      let input = Data.List.Split.splitOn ([toEnum 0]::String) (cr :: String)
      case input of
        (c:p:[]) -> do
          print "Successfully added record"
          putStrLn ""
          putStrLn ""
          x <- getCurrentTime
          atomically $ modifyTVar' pending (\records -> CommandRecord (cs c) x (cs p) : records)
          daemonLoop crFifoH pending ""
        _  -> do
          putStrLn $ (traceShow "Input^^^") (input !! 0)
          putStrLn $ (traceShow "Input^^^") (input !! 1)
          putStrLn $ (traceShow "cr^^^") cr
          print "Not sufficient input"
          putStrLn ""
          putStrLn ""
          daemonLoop crFifoH pending (cr)
    True -> do
      threadDelay (round $ 0.1 * (10^6))
      daemonLoop crFifoH (pending) s

savePendingRecordsToFs :: TVar [CommandRecord] -> TMVar Int -> IO ()
savePendingRecordsToFs x i = do
  let seconds = 1
  threadDelay (seconds * 10^6)
  runPeriodicallyBigDrift (fromIntegral seconds) $ do
    newEntries <- atomically $ swapTVar x []
    case newEntries of
      [] -> do
        print "No records to save to FS"
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

daemon :: Bool -> IO ()
daemon _ = do
  print "daemon"
  processPendingFiles True
  fileExist crFile >>= \case
    Right _ -> return ()
    Left _ -> do
      putStrLn "Creating new data file"
      encodeFile crFile ([] :: [CommandRecord])
  crFifoH <- openFile crFifo ReadMode
  x <- newTVarIO []
  i <- newTMVarIO (0 :: Int)
  _ <- forkIO $ savePendingRecordsToFs x i
  daemonLoop crFifoH x ""

processPendingFiles :: Bool -> IO ()
processPendingFiles _ = do
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

crFifo :: FilePath
crFifo = "/home/chris/.config/moscoviumOrange/commandRecord.fifo"
--crFifo = "test.fifo"

