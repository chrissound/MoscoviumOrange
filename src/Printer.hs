{-# OPTIONS -Wno-unused-imports #-}
module Printer where

import Rainbox
import Data.Function ((&))
import qualified Rainbow
import Rainbow.Types
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text, pack, unpack)

import Data.Binary
import Data.Binary.Orphans
import CommandRecord
import Daemon
import Control.Monad

import Data.Map.Strict (Map, fromList, (!))
import Data.List
import Data.List.Split
import Data.Foldable
import Data.String.Conversions

-- import Data.Time.Clock
-- import Data.Time.Format (defaultTimeLocale)
-- import Data.Time.Clock.POSIX
-- import System.Posix.Files
import System.Locale
import Data.Thyme.Clock
import Data.Thyme.Format

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

printRecords :: Bool -> Int -> IO ()
printRecords _ l = do
  crFile >>= decodeFileOrFail >>= \case
    Right p -> do
      pp <- getPendingRecords
      printRecords' (p ++ pp) l
    Left e -> error $ show e

printRecords' :: [CommandRecord] -> Int -> IO ()
printRecords' r l = do
  let tableV = fmap (renderCr) $ takeLastN l r
  mapM_ Rainbow.putChunk . toList $ render $ horizontalStationTable tableV

renderCr :: CommandRecord -> [String]
renderCr cr = [
    myFormatTime $ CommandRecord.timedate cr
  , cs $ CommandRecord.command cr
  , cs $ CommandRecord.path cr
  ]

myFormatTime :: FormatTime t => t -> String
myFormatTime fs = formatTime defaultTimeLocale "%x %r" fs
