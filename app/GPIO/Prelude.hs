{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module GPIO.Prelude  where

import Data.Proxy (Proxy(..))
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Catch
import Control.Monad.Trans.Reader
import Data.ByteString (ByteString)
import Data.Function (on)
import Foreign
import GHC.TypeLits (TypeError, ErrorMessage(..), Nat, type (+), KnownNat, natVal)
import GPIO.Raw
import System.Posix.IO
import System.Posix.Types

data Bias = PullUp | PullDown

data Edge = Rising | Falling

data EdgeDetection = EdgeDetection 
  { edge :: Edge
  , eventBufferSize :: Int
  }

data Pin = AsOutput Pi5Gpio Bool | AsInput Pi5Gpio Bias EdgeDetection

instance Eq Pin where
  (==) = (==) `on` pinOffset

data Pi5Gpio
    = GPIO0  | GPIO1  | GPIO2  | GPIO3  | GPIO4  | GPIO5  | GPIO6
    | GPIO7  | GPIO8  | GPIO9  | GPIO10 | GPIO11 | GPIO12 | GPIO13
    | GPIO14 | GPIO15 | GPIO16 | GPIO17 | GPIO18 | GPIO19 | GPIO20
    | GPIO21 | GPIO22 | GPIO23 | GPIO24 | GPIO25 | GPIO26 | GPIO27
    deriving (Eq, Ord, Show, Enum, Bounded)

data Lines = Lines { lFd :: Fd, lValPtr :: Ptr GpioV2LineValues, lReq :: LineRequest }

newtype LineM (reqs :: [(Pi5Gpio, Direction)]) a = LineM (ReaderT Lines IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

data Direction = Output | Input

type IndexOf :: Pi5Gpio -> Direction -> [(Pi5Gpio, Direction)] -> Nat
type family IndexOf pin dir reqs where
    IndexOf pin dir ('(pin, dir) ': _)   = 0
    IndexOf pin dir (_           ': rest) = 1 + IndexOf pin dir rest
    IndexOf pin dir '[]                   =
      TypeError ('ShowType pin
            ':<>: 'Text " was not requested with the required direction")

class KnownPin (pin :: Pi5Gpio) where
  pinVal :: Pi5Gpio

class KnownDir (dir :: Direction) where
  dirVal :: Direction

instance KnownDir 'Output
  where dirVal = Output

instance KnownDir 'Input
  where dirVal = Input

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

class KnownReq (reqs :: [(Pi5Gpio, Direction)]) where
  reqVal :: [(Pi5Gpio, Direction)]

instance KnownReq '[] where
  reqVal = []

instance (KnownPin pin, KnownDir dir, KnownReq rest)
      => KnownReq ('(pin, dir) ': rest) where
  reqVal = (pinVal @pin, dirVal @dir) : reqVal @rest

offset :: Pi5Gpio -> Int
offset = fromEnum
  
pinOffset :: Pin -> Int
pinOffset = \case
    AsOutput (offset -> o) _  -> o
    AsInput  (offset -> o) _ _ -> o

data LineRequest = LineRequest
  { consumer :: ByteString
  -- , fileDescriptor :: Fd
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
  , numAttrs = fromIntegral . sum . map (length . directionFlags) $ requests
  , attrs = concat $ zipWith configAttributes requests [0 ..]
  }

configAttributes :: Pin -> Int -> [GpioV2LineConfigAttribute]
configAttributes pin offsetIdx = flags : values
  where
      flags = 
          GpioV2LineConfigAttribute
            (GpioV2LineAttribute attributeIdFlags dirFlagSum)
            (bit offsetIdx)

      values = case pin of
          AsOutput (fromIntegral . (`shift` offsetIdx) . fromEnum -> initMask) _ -> 
              [ GpioV2LineConfigAttribute
                  (GpioV2LineAttribute attributeIdValues initMask)
                  (bit offsetIdx)] 
          _ -> []

      dirFlagSum = sum . map unGpioV2LineFlag . directionFlags $ pin

biasFlag :: Bias -> GpioV2LineFlag
biasFlag = \case
  PullUp -> flagBiasPullUp
  PullDown -> flagBiasPullDown

edgesFlag :: Edge -> GpioV2LineFlag
edgesFlag = \case
  Rising -> flagEdgeRising
  Falling -> flagEdgeFalling
    
directionFlags :: Pin -> [GpioV2LineFlag]
directionFlags = \case
  AsInput _ ipBias ipEdge  ->
      [ flagInput, biasFlag ipBias, edgesFlag (edge ipEdge) ]
  AsOutput _ _ ->
      [ flagOutput ]

writePin
    :: forall pin reqs. KnownNat (IndexOf pin 'Output reqs)
    => Bool -> LineM reqs ()
writePin val = LineM $ do
    Lines{lFd = Fd unFd, ..} <- ask
    let idx::Int = fromIntegral (natVal (Proxy @(IndexOf pin 'Output reqs)))
        shifted  = flip shift idx
    liftIO . poke lValPtr $ GpioV2LineValues (shifted (fromBool val)) (shifted 1)
    liftIO $ setValues unFd lValPtr

readPin
    :: forall pin reqs. KnownNat (IndexOf pin 'Input reqs)
    => LineM reqs Bool
readPin = LineM $ do
    Lines{lFd = Fd unFd, ..} <- ask
    let idx::Int = fromIntegral (natVal (Proxy @(IndexOf pin 'Input reqs)))
    liftIO $ poke lValPtr (GpioV2LineValues 0 (bit idx))
    liftIO $ getValues unFd lValPtr
    liftIO $ flip testBit idx . fromIntegral . bits <$> peek lValPtr


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
      lReq = mkLineRequest consumer (reqVal @reqs)

mkLineRequest :: ByteString -> [(Pi5Gpio, Direction)] -> LineRequest
mkLineRequest consumer pds = LineRequest consumer (map toPin pds)
    where
      toPin (p, Output) = AsOutput p False
      toPin (p, Input)  = AsInput p PullDown (EdgeDetection Rising 0)
