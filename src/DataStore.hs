module DataStore where

import Data.List
import Data.Binary ( decodeFileOrFail, encodeFile, decodeFile)
import Data.Bool (bool)
import System.Directory ( doesFileExist )
-- import System.IO
--     ( Handle, IOMode(ReadMode), hGetLine, openFile, hIsEOF )
--import Control.Concurrent.STM.TMVar

import System.Directory
import Data.Functor


import Control.Monad.State
import CommandRecord
import Data.Char (isDigit)

crFile :: IO FilePath
crFile = (getConfigDir <&> (++ "/commandRecord.data"))

crDirPending :: IO FilePath
crDirPending = (getConfigDir <&> (++ "/pending/"))

socketFile :: IO FilePath
socketFile = (getConfigDir <&> (++ "/monitor.soc"))


getConfigDir :: IO FilePath
getConfigDir = getXdgDirectory XdgConfig "moscoviumOrange"

getPendingRecords :: IO [CommandRecord]
getPendingRecords = do
  p <- crDirPending >>= getDirectoryContents
  let filtered = sort $ map read $ filter (all isDigit) p :: [Int]
  (forM filtered $ \i -> (crDirPending) <&> (++ show i) >>= decodeFile) >>= return . join

getSavedRecords :: IO [CommandRecord]
getSavedRecords = do
  crFile >>= decodeFileOrFail >>= \case
    Right p -> pure p
    Left e' -> error $ show e'

getAllRecords :: IO [CommandRecord]
getAllRecords = do
  crFile >>= decodeFileOrFail >>= \case
    Right p -> do
      (p ++) <$> getPendingRecords
    Left e' -> error $ show e'

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
