{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
module Daemon where

import Printer
import DataStore
import CommandRecord
import MoscoviumOrangePrelude
import Data.Thyme.Clock
import Filter

import Data.Foldable
import Data.Thyme.Clock ( getCurrentTime )
import Data.Binary ( encodeFile)
import Data.ByteString (null)
import Data.Bool (bool)
import System.Directory ( doesFileExist )
import Data.String.Conversions ( cs )
import Control.Concurrent.STM
import Data.Char (isDigit)
import Data.List (sort)
import Control.Concurrent
import System.IO
import System.Directory
import Data.Functor

import Network.Socket
import Network.Socket.ByteString as NBS

import Data.Aeson (decode)
import Types (Mosco, MoscState(..))
import qualified Types as MoscoTypes
import Control.Monad.IO.Class

import Control.Monad.State
import qualified Message.PrintFilterRecord as M_PFR
import Types

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

talkLoop :: TVar [CommandRecord] -> Socket -> Mosco b
talkLoop p sock = do
  (liftIO $ accept sock) >>= (talk p "" . fst)
  talkLoop p sock

posFilter :: String -> Socket -> Maybe (Mosco ())
posFilter s conn =
  case (decode $ cs s :: Maybe M_PFR.PrintFilterRecord) of
    Just x -> do
      pure $ do
        cr <- fmap crecords get
        pr <- liftIO getPendingRecords
        liftIO $ do
          output <- (printRecords'' $ filterRecords (M_PFR.filter x) (cr++pr) ) (M_PFR.length x) (M_PFR.rawJson x)
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
                  (:) (CommandRecord (cs $ Types.command x) ct (cs $ Types.path x))
                )
    _ -> Nothing

talk :: TVar [CommandRecord] ->  String -> Socket -> Mosco ()
talk pending s conn = do
  msg <- liftIO $ NBS.recv conn (2^16)
  case Data.ByteString.null msg of
    False -> do
      talk pending (s ++ cs msg) conn
    True -> do
      ct <- liftIO getCurrentTime
      case asum [
          fmap liftIO (posMsg s pending ct)
        , posFilter s conn
        ] of
        Just x -> x
        Nothing -> liftIO $ do
          putStrLn $ "Found invalid daemon message:"
          sendAll conn "Found invalid daemon message:"
      liftIO $ close conn

daemon :: Mosco ()
daemon = do
  liftIO $ do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
  ms <- get
  let mc = moscConf ms
  (x,i) <- liftIO $ do
    x <- newTVarIO []
    i <- newTMVarIO (0 :: Int)
    pure (x,i)
  _ <- liftIO $ do
    processPendingFiles
    print "Ruining daemon 1"
    forkIO $ savePendingRecordsToFs x i
  soc <- liftIO $ do
    doesFileExist (MoscoTypes.socket mc) >>= bool
      (pure ())
      (do
          print "Removing file..."
          removeFile $ MoscoTypes.socket mc)
    soc <- Network.Socket.socket AF_UNIX Stream 0
    bind soc . SockAddrUnix . MoscoTypes.socket $ mc
    listen soc maxListenQueue
    pure soc
  liftIO $ print "Running daemon 2"
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

