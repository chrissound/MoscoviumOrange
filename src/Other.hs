module Other where

import Control.Lens
import Data.Thyme.Clock
import Data.Thyme.Calendar

makeUtcTime :: Year -> Month -> DayOfMonth -> Int -> UTCTime
makeUtcTime y m d x =  UTCTime ((YearMonthDay y m d)^.from gregorian) (toEnum x) ^. from utcTime
