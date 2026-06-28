{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Example where

import GPIO.Prelude
import Control.Concurrent
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Catch
import Control.Monad

type BlinkReqs = '[ '(GPIO3, Output), '(GPIO2, Input) ]

blink :: IO ()
blink = do
    withLine @BlinkReqs "Example:blink" $
        (forever $ do
            writePin @GPIO3 False
            liftIO $ threadDelay 500_000
            _ <- readPin @GPIO2
            liftIO $ threadDelay 500_000)
          `finally` writePin @GPIO3 False
