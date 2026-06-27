{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module GPIO.Prelude  where


import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Data.Bits (shift)
import Data.List (elemIndex)
import Data.ByteString (ByteString)
import Data.Function (on)
import Data.Set qualified as Set
import Foreign
import GPIO.Raw
import System.Posix.IO
import System.Posix.Types

data Bias = PullUp | PullDown

data Edge = Rising | Falling

data EdgeDetection = EdgeDetection 
  { edge :: Edge
  , eventBufferSize :: Int
  }

data OutPin = OutPin {opPin :: Pi5Gpio, opVal :: Bool}

data InPin = InPin {ipPin :: Pi5Gpio, ipBias :: Bias, ipEdge :: EdgeDetection}

data Pin = AsOutput OutPin | AsInput InPin

instance Eq Pin where
  (==) = (==) `on` pinOffset

data Pi5Gpio
    = GPIO0  | GPIO1  | GPIO2  | GPIO3  | GPIO4  | GPIO5  | GPIO6
    | GPIO7  | GPIO8  | GPIO9  | GPIO10 | GPIO11 | GPIO12 | GPIO13
    | GPIO14 | GPIO15 | GPIO16 | GPIO17 | GPIO18 | GPIO19 | GPIO20
    | GPIO21 | GPIO22 | GPIO23 | GPIO24 | GPIO25 | GPIO26 | GPIO27
    deriving (Eq, Ord, Show, Enum, Bounded)

offset :: Pi5Gpio -> Int
offset = fromEnum
  
pinOffset :: Pin -> Int
pinOffset = \case
    AsOutput (offset . opPin -> o) -> o
    AsInput  (offset . ipPin -> o) -> o

data LineRequest = LineRequest
  { consumer :: ByteString
  -- , fileDescriptor :: Fd
  , requests :: Set.Set Pin
  }

gpioV2LineRequest :: LineRequest -> GpioV2LineRequest
gpioV2LineRequest lr@(LineRequest{..}) = GpioV2LineRequest
  { consumer
  , fileDescriptor = 0
  , numLines = fromIntegral (Set.size requests)
  , eventBufferSize = 0
  , config = gpioV2LineConfig lr
  , offsets =  map (fromIntegral . pinOffset) (Set.toList requests)
  }

gpioV2LineConfig :: LineRequest -> GpioV2LineConfig 
gpioV2LineConfig LineRequest{..} = GpioV2LineConfig
  { flags = 0
  , numAttrs = fromIntegral . sum . map (length . directionFlags) . Set.elems $ requests
  , attrs = concat $ zipWith configAttributes (Set.elems requests) [0 ..]
  }

configAttributes :: Pin -> Int -> [GpioV2LineConfigAttribute]
configAttributes pin offsetIdx = flags : values
  where
      flags = 
          GpioV2LineConfigAttribute
            (GpioV2LineAttribute attributeIdFlags dirFlagSum)
            (fromIntegral offsetIdx)

      values = case pin of
          AsOutput (fromIntegral . (`shift` offsetIdx) . fromEnum . opPin -> initMask) -> 
              [ GpioV2LineConfigAttribute
                  (GpioV2LineAttribute attributeIdValues initMask)
                  (fromIntegral offsetIdx)] 
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
  AsInput InPin{..} ->
      [ flagInput, biasFlag ipBias, edgesFlag (edge ipEdge) ]
  AsOutput _ ->
      [ flagOutput ]

data Lines = Lines { lFd :: Fd, lValPtr :: Ptr GpioV2LineValues, lReq :: LineRequest }
type LineM = ReaderT Lines IO

-- TODO: hinder using a pin that wasn't requested
writePin :: OutPin -> LineM ()
writePin op@OutPin{..} = do
    Lines{lFd = Fd unFd, ..} <- ask
    let Just idx = elemIndex (AsOutput op) (Set.toList (requests lReq))
        shifted    = flip shift idx
    liftIO . poke lValPtr $ GpioV2LineValues (shifted (fromBool opVal)) (shifted 1)
    liftIO $ setValues unFd lValPtr

withChip :: (Fd -> IO ()) -> IO ()
withChip = bracket
  (openFd "/dev/gpiochip0" ReadWrite defaultFileFlags)
  closeFd

withLine :: LineRequest -> LineM () -> IO ()
withLine lReq act = 
    withChip $ \(Fd chipFd) ->
        with (gpioV2LineRequest lReq) $ \reqPtr -> do
            bracket 
                (do requestLines chipFd reqPtr
                    lineFd <- Fd . fromIntegral . fileDescriptor <$> peek reqPtr
                    pure lineFd)
                closeFd
                (\lFd -> alloca $ \lValPtr -> runReaderT act Lines{..})
