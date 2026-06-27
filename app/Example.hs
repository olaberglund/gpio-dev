{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Example where

import GPIO.Prelude
import Data.Set qualified as Set
import Control.Concurrent
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Catch
import Control.Monad

blink :: IO ()
blink = do
    let req = LineRequest "Example:blink" $ Set.singleton (AsOutput (OutPin GPIO3 False))
    withLine req $
        (forever $ do
            writePin (OutPin GPIO3 False)
            liftIO $ threadDelay 500_000
            writePin (OutPin GPIO3 True)
            liftIO $ threadDelay 500_000) 
          `finally` writePin (OutPin GPIO3 False)
