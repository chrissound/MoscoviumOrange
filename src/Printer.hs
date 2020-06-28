{-# OPTIONS -Wno-unused-imports #-}
module Printer where

import Rainbox
import Data.Function ((&))
import Rainbow
import Rainbow.Types
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text, pack, unpack)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BLC8

import Data.Aeson (encode)
import Data.Aeson.Encode.Pretty

import qualified Data.Binary as Bin
import Data.Binary.Orphans
import CommandRecord
import Control.Monad

import Data.Map.Strict (Map, fromList, (!))
import Data.List
import Data.List.Split
import Data.Foldable
import Data.String.Conversions
import DataStore
import Network.Socket hiding (send)
import Network.Socket.ByteString as NBS


-- import Data.Time.Clock
-- import Data.Time.Format (defaultTimeLocale)
-- import Data.Time.Clock.POSIX
-- import System.Posix.Files
import System.Locale
import Data.Thyme.Clock
import Data.Thyme.Format
import Types
import Filter
import Filter
import qualified Message.PrintFilterRecord as M_PFR
import Control.Monad.IO.Class
import Control.Monad.State
import System.Console.Terminfo.Base
import System.IO

stationColumn :: [(String, Rainbow.Radiant, Alignment Vertical)] -> Seq Cell
stationColumn = fcol . xyz . Seq.fromList . fmap (\(v,c,a) -> myCell defaultText c a (pack v))

horizontalStationTable :: [[String]] -> Rainbox.Box Rainbox.Vertical
horizontalStationTable vvv
  = Rainbox.tableByRows
  . Seq.fromList
  $ (stationColumn <$> (fmap (\x -> zip3 x (colssss) aliii) vvv ))

aliii :: [Alignment Vertical]
aliii = [
    Rainbox.left
    , Rainbox.left
    , Rainbox.left
        ]

colssss :: [Rainbow.Radiant]
colssss = [
    Rainbow.blue
  , Rainbow.white
  , Rainbow.green
  ]

fcol :: Seq Cell -> Seq Cell
fcol =
    Seq.adjust (\x -> x { _background = defaultText}) 0

xyz :: Seq Cell -> Seq Cell
xyz = (Seq.intersperse (separator defaultText 1))

myCell :: Rainbow.Radiant -> Rainbow.Radiant -> Alignment Vertical -> Text -> Rainbox.Cell
myCell b f a vv = Rainbox.Cell v Rainbox.top a b
  where
    v = Seq.singleton . Seq.singleton $ (Rainbow.chunk vv & Rainbow.fore f)

defaultText :: Rainbow.Radiant
defaultText = Radiant (Color Nothing) (Color Nothing)

takeLastN :: Int -> [a] -> [a]
takeLastN n = reverse . take n . reverse

printRecords :: Bool -> Int -> Bool -> IO ()
printRecords _ l rj = do
  crFile >>= Bin.decodeFileOrFail >>= \case
    Right p -> do
      pp <- getPendingRecords
      printRecords' (p ++ pp) l rj
    Left e -> error $ show e

printRecords' :: [CommandRecord] -> Int -> Bool -> IO ()
printRecords' = (fmap $ fmap (>>= BLC8.putStrLn . cs)) <$> printRecords''

printRecords'' :: [CommandRecord] -> Int -> Bool -> IO BS.ByteString
printRecords'' r l True = do
  let tableV = fmap (renderCr) $ takeLastN l r
  pure $ cs $ encodePretty tableV
printRecords'' r l False = do
  -- setupTermFromEnv >>= print
  -- f <- byteStringMakerFromEnvironment
  let f = toByteStringsColors256
  let tableV = renderCr <$> takeLastN l r
  pure $ mconcat $ chunksToByteStrings f
    $ toList $ render $ horizontalStationTable $ tableV

renderCr :: CommandRecord -> [String]
renderCr cr = [
    myFormatTime $ CommandRecord.timedate cr
  , cs $ CommandRecord.command cr
  , cs $ CommandRecord.path cr
  ]

myFormatTime :: FormatTime t => t -> String
myFormatTime fs = formatTime defaultTimeLocale timeFormatThingy fs

timeFormatThingy :: String
timeFormatThingy = "%x %r"

printFilterRecords
  :: [Text]
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text -- path
  -> [Text]
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text -- command
  -> Maybe UTCTime
  -> Maybe UTCTime -- before / after
  -> Int
  -> Bool
  -> Mosco ()
printFilterRecords pa pb pc pd ca cb cc cd tb ta l rj = do
  let filter' = Filter { pathContains    = pa
                       , pathPrefix      = pb
                       , pathSuffix      = pc
                       , pathEqual       = pd
                       , commandContains = ca
                       , commandPrefix   = cb
                       , commandSuffix   = cc
                       , commandEqual    = cd
                       , before          = tb
                       , after           = ta
                       }
  let pfr = M_PFR.PrintFilterRecord { M_PFR.filter = filter', M_PFR.length = l, M_PFR.rawJson = rj }
  ms <- get
  liftIO $ do
    withSocketsDo $ do
      soc <- Network.Socket.socket AF_UNIX Stream 0
      connect soc (SockAddrUnix $ Types.socket $ moscConf ms)
      sendAll soc $ cs $ encode pfr
      shutdown soc ShutdownSend
      h <- socketToHandle soc ReadMode
      msg <- BS.hGetContents h
      putStrLn $ cs msg
