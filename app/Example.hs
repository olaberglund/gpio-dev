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
    withLine @'[ '( 'GPIO3, 'Output) ] "Example:blink" $
        (forever $ do
            writePin @'GPIO3 False
            liftIO $ threadDelay 500_000
            writePin @'GPIO3 True
            liftIO $ threadDelay 500_000)
          `finally` writePin @'GPIO3 False
