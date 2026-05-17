{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Posix.IO
import System.Posix.Types
import Foreign
import Control.Concurrent
import GPIO.Raw
import Control.Exception
import Control.Monad

main :: IO ()
main = bracket
  (openFd "/dev/gpiochip0" ReadWrite defaultFileFlags)
  closeFd
  $ \(Fd chipFd) -> do
      let req = GpioV2LineRequest
            { offsets = [3]
            , consumer = "gpio_blink"
            , config = GpioV2LineConfig output 0
            , numLines = 1
            , eventBufferSize = 0
            , fileDescriptor = 0
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

