{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Example where

import GPIO.Prelude
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever, void)

blink :: IO ()
blink =
    withLine @'[Out G4] $ forever do
        togglePin @G4
        liftIO (threadDelay 500_000)


click :: IO ()
click = withLine @'[ 'Pin G4 (OutSpec True), In G17] do
           void (nextEvents @G17)
