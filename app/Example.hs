{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Example where

import Gpio.Line
import Gpio.Pin
import Control.Concurrent (threadDelay, killThread)
import Control.Monad.Catch (catch)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever, void)
import Control.Concurrent.MVar
import Control.Exception (throwTo, AsyncException(ThreadKilled))
import Control.Monad.Trans.Reader

blink :: IO ()
blink =
    withLine @'[Out G17] $ forever do
        togglePin @G17
        liftIO (threadDelay 500_000)


click :: IO ()
click = withLine @'[ 'Pin G17 (OutSpec True), In G4] do
           void (nextEvents @G4)

blinkClickToStop :: IO ()
blinkClickToStop = do
    withLine @'[ 'Pin G17 (OutSpec True), In G4] do
        blinkTid <- forkLineM . forever $ do
            togglePin @G17
            liftIO (threadDelay 500_000)
        nextEvents @G4
        liftIO (killThread blinkTid)
