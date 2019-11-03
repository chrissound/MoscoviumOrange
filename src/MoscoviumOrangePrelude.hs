module MoscoviumOrangePrelude where

import           Control.Concurrent ( threadDelay )
import           Control.Monad ( forever )

ifAllM :: [IO Bool] -> IO Bool
ifAllM = fmap (Prelude.all (== True)) . sequence

-- | Run @action@ every @period@ seconds.
runPeriodicallyBigDrift :: Double -> IO () -> IO ()
runPeriodicallyBigDrift period action = forever $ do
  action
  threadDelay (round $ period * 10 ** 6)
