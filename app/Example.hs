{-# LANGUAGE OverloadedStrings #-}

module Example where

import System.Posix.IO
import System.Posix.Types
import Control.Monad.STM
import Foreign
import Control.Concurrent
import GPIO.Raw
import Control.Exception
import Control.Concurrent.STM.TVar
import Control.Monad
import System.IO

blink :: IO ()
blink = withChip $ \(Fd chipFd) -> do
    let req = emptyReq
          { offsets = [3]
          , consumer = "gpio_blink"
          , config = GpioV2LineConfig output 0
          , numLines = 1
          }
    with req $ \reqPtr -> do
      requestLines chipFd reqPtr
      lineFd <- fromIntegral . fileDescriptor <$> peek reqPtr
      bracket (pure (Fd lineFd))
        closeFd
        $ \(Fd lf) -> do
            alloca $ \valPtr -> do
              let writeBit b = poke valPtr (GpioV2LineValues b 1) >> setValues lf valPtr
              (forever $ do
                  writeBit 1 >> setValues lf valPtr
                  threadDelay 500_000
                  writeBit 0 >> setValues lf valPtr
                  threadDelay 500_000) 
                `finally` writeBit 0

toggleLight :: IO ()
toggleLight = do
    keyVar <- newTVarIO False
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False

    _ <- forkIO (getKey keyVar)

    withChip $ \(Fd chipFd) -> do
        let req = emptyReq
              { offsets = [3]
              , consumer = "gpio_blink"
              , config = GpioV2LineConfig output 0
              , numLines = 1
              }
        with req $ \reqPtr -> do
          requestLines chipFd reqPtr
          lineFd <- fromIntegral . fileDescriptor <$> peek reqPtr
          bracket (pure (Fd lineFd))
            closeFd
            $ \(Fd lf) -> do
                alloca $ \valPtr -> do
                  let writeBit b = poke valPtr (GpioV2LineValues b 1) >> setValues lf valPtr
                      loop lastChar = do
                        c <- atomically $ do
                            c <- readTVar keyVar
                            if c == lastChar then retry else pure c
                        writeBit (fromBool c)
                        loop c
                  loop False `finally` writeBit 0

getKey :: TVar Bool -> IO ()
getKey tv = forever $ do
  keys <- getChar
  case keys of
    'k' -> atomically (modifyTVar' tv not)
    _ -> pure ()

withChip :: (Fd -> IO ()) -> IO ()
withChip act = bracket
  (openFd "/dev/gpiochip0" ReadWrite defaultFileFlags)
  closeFd
  act

