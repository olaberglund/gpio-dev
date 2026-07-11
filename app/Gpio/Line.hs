{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Gpio.Line where

import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Data.Coerce (coerce)
import Foreign
import Gpio.Ioctl
import Gpio.Pin
import System.Posix.IO
import System.Posix.Types

writePin
    :: forall pin reqs. RequestedOutput pin reqs
    => Bool -> LineM reqs ()
writePin val = LineM do
    Lines{lFd = Fd unFd, ..} <- ask
    let shifted  = flip shift (pinIndex @pin @reqs)
    liftIO . poke lValPtr . GpioV2LineValues (shifted (fromBool val)) $ shifted 1
    liftIO $ setValues unFd lValPtr

readPin
    :: forall pin reqs. Requested pin reqs
    => LineM reqs Bool
readPin = LineM do
    Lines{lFd = Fd unFd, ..} <- ask
    let idx = pinIndex @pin @reqs
    liftIO . poke lValPtr . GpioV2LineValues 0 . bit $ idx
    liftIO $ getValues unFd lValPtr
    liftIO $ flip testBit idx . fromIntegral . bits <$> peek lValPtr

togglePin 
    :: forall pin reqs. RequestedOutput pin reqs
    => LineM reqs ()
togglePin = readPin @pin >>= writePin @pin . not

withChip :: (Fd -> IO ()) -> IO ()
withChip = bracket
  (openFd "/dev/gpiochip0" ReadWrite defaultFileFlags)
  closeFd

withLineNoRestore
    :: forall reqs. KnownReq reqs
    => LineM reqs () -> IO ()
withLineNoRestore (LineM act) =
      withChip $ \(Fd chipFd) ->
          with (gpioV2LineRequest lReq) \reqPtr -> do
              bracket
                  (requestLines chipFd reqPtr >> Fd . fromIntegral . fileDescriptor <$> peek reqPtr)
                  closeFd
                  (\lFd -> alloca $ \lValPtr -> runReaderT act Lines{..})
    where
      lReq = LineRequest "RPI5" (reqVal @reqs)

withLine :: forall reqs. (KnownReq reqs, KnownReq (AsInput reqs), ValidReconfigure reqs (AsInput reqs))
    => LineM reqs () -> IO ()
withLine = withLineNoRestore . flip finally (resetPins @reqs)

resetPins :: forall reqs. (KnownReq reqs, KnownReq (AsInput reqs), ValidReconfigure reqs (AsInput reqs))
    => LineM reqs ()
resetPins = reconfigure @(AsInput reqs) (pure ())

reconfigure :: forall newReqs reqs. (KnownReq reqs, KnownReq newReqs, ValidReconfigure reqs newReqs )
    => LineM newReqs () -> LineM reqs ()
reconfigure (LineM act) = LineM do
    lines <- ask
    let new = LineRequest lines.lReq.consumer $ reqVal @newReqs
    liftIO $ with (gpioV2LineConfig new) (setLineConfigs (coerce lines.lFd))
    local (\l -> l{lReq = new}) act

nextEvents
    :: forall pin reqs. RequestedInput pin reqs
    => LineM reqs [GpioV2LineEvent]
nextEvents = LineM do
    Lines{lFd = fd@(Fd unFd), ..} <- ask
    let sz  = sizeOf (undefined :: GpioV2LineEvent)
        cap = 16
    liftIO $ allocaBytes (cap * sz) $ \buf -> do
        threadWaitRead fd
        n <- readEvents unFd (fromIntegral (cap * sz)) buf
        peekArray (fromIntegral n `div` sz) buf

forkLineM :: LineM reqs () -> LineM reqs ThreadId
forkLineM (LineM act) = LineM do
    lines <- ask
    liftIO $ forkIO (runReaderT act lines)

