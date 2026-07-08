{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module GPIO.Prelude  where

import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Reader
import Data.ByteString (ByteString)
import Data.Function (on)
import Data.Kind (Constraint)
import Data.Proxy (Proxy(..))
import Foreign
import GHC.TypeLits (TypeError, ErrorMessage(..), Nat, type (+), KnownNat, natVal)
import GPIO.Raw
import System.Posix.IO
import System.Posix.Types

data Bias = PullUp | PullDown | PullUpDown
  deriving Eq

data Edge = Rising | Falling | RisingAndFalling
  deriving Eq

data PinSpec = OutSpec Bool | InSpec Bias Edge
  deriving Eq

data Pin = Pin { pinGpio :: Pi5Gpio, pinSpec ::  PinSpec }

instance Eq Pin where
  (==) = (==) `on` pinOffset

data Pi5Gpio
    = GPIO0  | GPIO1  | GPIO2  | GPIO3  | GPIO4  | GPIO5  | GPIO6
    | GPIO7  | GPIO8  | GPIO9  | GPIO10 | GPIO11 | GPIO12 | GPIO13
    | GPIO14 | GPIO15 | GPIO16 | GPIO17 | GPIO18 | GPIO19 | GPIO20
    | GPIO21 | GPIO22 | GPIO23 | GPIO24 | GPIO25 | GPIO26 | GPIO27
    deriving (Eq, Ord, Show, Enum, Bounded)

data Lines = Lines { lFd :: Fd, lValPtr :: Ptr GpioV2LineValues, lReq :: LineRequest }

newtype LineM (reqs::[Pin]) a = LineM (ReaderT Lines IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

data Direction = Output | Input

type PinIndex :: Pi5Gpio -> [Pin] -> Nat
type family PinIndex pin reqs where
  PinIndex pin ('Pin pin _ ': _)    = 0
  PinIndex pin (_   ': rest) = 1 + PinIndex pin rest
  PinIndex pin '[] =
    TypeError ('ShowType pin ':<>: 'Text " was not requested")

type DirectedPinIndex :: Pi5Gpio -> Direction -> [Pin] -> Nat
type family DirectedPinIndex pin dir reqs where
    DirectedPinIndex pin 'Input  ('Pin pin ('InSpec  _ _) ': _) = 0
    DirectedPinIndex pin 'Output ('Pin pin ('OutSpec _)   ': _) = 0
    DirectedPinIndex pin dir (_ ': rest) = 1 + DirectedPinIndex pin dir rest
    DirectedPinIndex pin dir '[] =
      TypeError ('ShowType pin
            ':<>: 'Text " was not requested with the required direction (" ':<>: 'ShowType dir ':<>: 'Text ")" )


type FixedPull :: Pi5Gpio -> Maybe Bias
type family FixedPull pin where
    FixedPull 'GPIO2 = 'Just 'PullUp
    FixedPull 'GPIO3 = 'Just 'PullUp
    FixedPull _      = 'Nothing

type ValidBias :: Pi5Gpio -> Bias -> Constraint
type family ValidBias pin bias where
    ValidBias pin bias = CheckPull pin bias (FixedPull pin)

type CheckPull :: Pi5Gpio -> Bias -> Maybe Bias -> Constraint
type family CheckPull pin bias fixed where
    CheckPull pin 'PullDown ('Just 'PullUp) =
        TypeError ('ShowType pin ':<>: 'Text " has a fixed hardware pull-up. PullDown is impossible.")
    CheckPull _ _ _ = ()

class Requested (pin :: Pi5Gpio) (reqs :: [Pin]) where
  pinIndex :: Int

instance KnownNat (PinIndex pin reqs) => Requested pin reqs where
  pinIndex = fromIntegral (natVal (Proxy @(PinIndex pin reqs)))

class Requested pin reqs => RequestedAs (dir :: Direction) pin reqs

instance ( Requested pin reqs
         , DirectedPinIndex pin dir reqs ~ PinIndex pin reqs )
      => RequestedAs dir pin reqs

type RequestedOutput = RequestedAs 'Output

type RequestedInput = RequestedAs 'Input

class KnownPin (pin :: Pi5Gpio) where
  pinVal :: Pi5Gpio

instance KnownPin 'GPIO0  where pinVal = GPIO0
instance KnownPin 'GPIO1  where pinVal = GPIO1
instance KnownPin 'GPIO2  where pinVal = GPIO2
instance KnownPin 'GPIO3  where pinVal = GPIO3
instance KnownPin 'GPIO4  where pinVal = GPIO4
instance KnownPin 'GPIO5  where pinVal = GPIO5
instance KnownPin 'GPIO6  where pinVal = GPIO6
instance KnownPin 'GPIO7  where pinVal = GPIO7
instance KnownPin 'GPIO8  where pinVal = GPIO8
instance KnownPin 'GPIO9  where pinVal = GPIO9
instance KnownPin 'GPIO10 where pinVal = GPIO10
instance KnownPin 'GPIO11 where pinVal = GPIO11
instance KnownPin 'GPIO12 where pinVal = GPIO12
instance KnownPin 'GPIO13 where pinVal = GPIO13
instance KnownPin 'GPIO14 where pinVal = GPIO14
instance KnownPin 'GPIO15 where pinVal = GPIO15
instance KnownPin 'GPIO16 where pinVal = GPIO16
instance KnownPin 'GPIO17 where pinVal = GPIO17
instance KnownPin 'GPIO18 where pinVal = GPIO18
instance KnownPin 'GPIO19 where pinVal = GPIO19
instance KnownPin 'GPIO20 where pinVal = GPIO20
instance KnownPin 'GPIO21 where pinVal = GPIO21
instance KnownPin 'GPIO22 where pinVal = GPIO22
instance KnownPin 'GPIO23 where pinVal = GPIO23
instance KnownPin 'GPIO24 where pinVal = GPIO24
instance KnownPin 'GPIO25 where pinVal = GPIO25
instance KnownPin 'GPIO26 where pinVal = GPIO26
instance KnownPin 'GPIO27 where pinVal = GPIO27

class KnownReq (reqs :: [Pin]) where
  reqVal :: [Pin]
instance KnownReq '[] where
  reqVal = []

class KnownBias (b :: Bias) where biasVal :: Bias
instance KnownBias 'PullUp where biasVal = PullUp
instance KnownBias 'PullDown where biasVal = PullDown

class KnownEdge (e :: Edge) where edgeVal :: Edge
instance KnownEdge 'Rising where edgeVal = Rising
instance KnownEdge 'Falling where edgeVal = Falling

class KnownBool (b :: Bool) where boolVal :: Bool
instance KnownBool 'True  where boolVal = True
instance KnownBool 'False where boolVal = False

instance (KnownPin pin, KnownBias bias, KnownEdge edg, ValidBias pin bias, KnownReq rest)
      => KnownReq ('Pin pin ('InSpec bias edg) ': rest) where
  reqVal = Pin (pinVal @pin) (InSpec (biasVal @bias) (edgeVal @edg)) : reqVal @rest

instance (KnownPin pin, KnownBool val, KnownReq rest)
      => KnownReq ('Pin pin ('OutSpec val) ': rest) where
  reqVal = Pin (pinVal @pin) (OutSpec (boolVal @val)) : reqVal @rest


pinOffset :: Pin -> Int
pinOffset = fromEnum . pinGpio

data LineRequest = LineRequest
  { consumer :: ByteString
  , requests :: [Pin]
  }

gpioV2LineRequest :: LineRequest -> GpioV2LineRequest
gpioV2LineRequest lr@(LineRequest{..}) = GpioV2LineRequest
  { consumer
  , fileDescriptor = 0
  , numLines = fromIntegral (length requests)
  , eventBufferSize = 0
  , config = gpioV2LineConfig lr
  , offsets =  map (fromIntegral . pinOffset) requests
  }

gpioV2LineConfig :: LineRequest -> GpioV2LineConfig 
gpioV2LineConfig LineRequest{..} = GpioV2LineConfig
  { flags = 0
  , numAttrs = fromIntegral (length attrs)
  , attrs
  }
  where
      attrs = concat $ zipWith configAttributes requests [0 ..]

configAttributes :: Pin -> Int -> [GpioV2LineConfigAttribute]
configAttributes (Pin _ spec) offsetIdx = flags : values
  where
      flags =
          GpioV2LineConfigAttribute
            (GpioV2LineAttribute attributeIdFlags dirFlagSum)
            (bit offsetIdx)

      values = case spec of
          OutSpec v ->
              [ GpioV2LineConfigAttribute
                  (GpioV2LineAttribute attributeIdValues (initMask v))
                  (bit offsetIdx) ]
          InSpec{} -> []

      initMask v = fromIntegral (fromEnum v `shift` offsetIdx)

      dirFlagSum = sum . map unGpioV2LineFlag . directionFlags $ spec

biasFlag :: Bias -> GpioV2LineFlag
biasFlag = \case
  PullUp -> flagBiasPullUp
  PullDown -> flagBiasPullDown

edgesFlag :: Edge -> GpioV2LineFlag
edgesFlag = \case
  Rising -> flagEdgeRising
  Falling -> flagEdgeFalling
    
directionFlags :: PinSpec -> [GpioV2LineFlag]
directionFlags = \case
  InSpec isBias isEdge  -> [ flagInput, biasFlag isBias, edgesFlag isEdge ]
  OutSpec _ -> [ flagOutput ]

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

withLine
    :: forall reqs. KnownReq reqs
    => ByteString -> LineM reqs () -> IO ()
withLine consumer (LineM act) =
      withChip $ \(Fd chipFd) ->
          with (gpioV2LineRequest lReq) $ \reqPtr -> do
              bracket
                  (do requestLines chipFd reqPtr
                      Fd . fromIntegral . fileDescriptor <$> peek reqPtr)
                  closeFd
                  (\lFd -> alloca $ \lValPtr -> runReaderT act Lines{..})
    where
      lReq = LineRequest consumer (reqVal @reqs)

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
