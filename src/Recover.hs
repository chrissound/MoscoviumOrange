{-# LANGUAGE OverloadedStrings #-}
module Recover where

import Types
-- import qualified Types as MoscoTypes
import Control.Monad.IO.Class
import Control.Monad.State as State
import           Data.Binary as Binary
import CommandRecord
-- import           Data.Binary.Orphans
import qualified Data.ByteString.Lazy as LBS (readFile, length)

recover :: Mosco ()
recover = do
  ms <- State.get
  liftIO $ do
    let z = (datadir $ moscConf ms)
    print z
    fuck2 <- LBS.readFile z
    case (decodeOrFail fuck2) of
      Right (_,_,a) -> print $ length (a :: [CommandRecord])
      -- Left (x,xx,xxx) -> print "No"
      Left (x,xx,s) -> do
        print x
        print xx
        print s
        print $ LBS.length fuck2
        print "No"
           -- )
    -- print zzz
