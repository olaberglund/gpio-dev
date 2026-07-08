{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Example where

import GPIO.Prelude
import Control.Concurrent
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Catch
import Control.Monad

type BlinkReqs = '[ 'Pin GPIO2 ('OutSpec 'True) ]

blink :: IO ()
blink = do
    withLine @BlinkReqs "Example:blink" $
        (do
            r1 <- readPin @GPIO2
            liftIO $ putStrLn $ "Read is: " <> show r1
            )
          -- `finally` writePin @GPIO17 False
