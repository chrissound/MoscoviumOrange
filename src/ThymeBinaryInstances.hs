{-# OPTIONS -Wno-orphans #-}
module ThymeBinaryInstances where

import Data.Binary
import Data.Thyme.Internal.Micro
import Data.Thyme.Clock

instance Binary UTCTime
instance Binary NominalDiffTime
instance Binary Micro
