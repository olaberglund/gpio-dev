{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Gpio.Pin where

import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader
import Data.ByteString (ByteString)
import Data.Function (on)
import Data.Kind (Constraint)
import Data.Proxy (Proxy(..))
import Foreign
import GHC.TypeLits
import Gpio.Ioctl
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
    = G0  | G1  | G2  | G3  | G4  | G5  | G6
    | G7  | G8  | G9  | G10 | G11 | G12 | G13
    | G14 | G15 | G16 | G17 | G18 | G19 | G20
    | G21 | G22 | G23 | G24 | G25 | G26 | G27
    deriving (Eq, Ord, Show, Enum, Bounded)

data LineRequest = LineRequest
  { consumer :: ByteString
  , requests :: [Pin]
  }

data Lines = Lines { lFd :: Fd, lValPtr :: Ptr GpioV2LineValues, lReq :: LineRequest }

newtype LineM (reqs::[Pin]) a = LineM (ReaderT Lines IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

data Direction = Output | Input

type In :: Pi5Gpio -> Pin
type family In pin where
    In pin = 'Pin pin (InSpec PullDown Rising)

type Out :: Pi5Gpio -> Pin
type family Out pin where
    Out pin = 'Pin pin (OutSpec False)


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
    FixedPull 'G2 = 'Just 'PullUp
    FixedPull 'G3 = 'Just 'PullUp
    FixedPull _      = 'Nothing

type IsSubsetOf :: [Pin] -> [Pin] -> Symbol -> Constraint
type family IsSubsetOf xs ys label where
    IsSubsetOf '[] _ _ = ()
    IsSubsetOf ('Pin p _ ': rest) ys label =
        (RequireElem p ys label, IsSubsetOf rest ys label)

type RequireElem :: Pi5Gpio -> [Pin] -> Symbol -> Constraint
type family RequireElem pin pins label where
    RequireElem pin ('Pin pin _ ': rest) label = ()
    RequireElem pin (_ ': rest) label = RequireElem pin rest label
    RequireElem pin '[] label =
        TypeError ('ShowType pin ':<>: 'Text " is missing from the " ':<>: 'Text label ':<>: 'Text " configuration.")

type ValidReconfigure reqs newReqs =
        (IsSubsetOf reqs newReqs "new", IsSubsetOf newReqs reqs "old")

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

instance KnownPin 'G0  where pinVal = G0
instance KnownPin 'G1  where pinVal = G1
instance KnownPin 'G2  where pinVal = G2
instance KnownPin 'G3  where pinVal = G3
instance KnownPin 'G4  where pinVal = G4
instance KnownPin 'G5  where pinVal = G5
instance KnownPin 'G6  where pinVal = G6
instance KnownPin 'G7  where pinVal = G7
instance KnownPin 'G8  where pinVal = G8
instance KnownPin 'G9  where pinVal = G9
instance KnownPin 'G10 where pinVal = G10
instance KnownPin 'G11 where pinVal = G11
instance KnownPin 'G12 where pinVal = G12
instance KnownPin 'G13 where pinVal = G13
instance KnownPin 'G14 where pinVal = G14
instance KnownPin 'G15 where pinVal = G15
instance KnownPin 'G16 where pinVal = G16
instance KnownPin 'G17 where pinVal = G17
instance KnownPin 'G18 where pinVal = G18
instance KnownPin 'G19 where pinVal = G19
instance KnownPin 'G20 where pinVal = G20
instance KnownPin 'G21 where pinVal = G21
instance KnownPin 'G22 where pinVal = G22
instance KnownPin 'G23 where pinVal = G23
instance KnownPin 'G24 where pinVal = G24
instance KnownPin 'G25 where pinVal = G25
instance KnownPin 'G26 where pinVal = G26
instance KnownPin 'G27 where pinVal = G27

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

type AsInput :: [Pin] -> [Pin]
type family AsInput pins where
  AsInput '[] = '[]
  AsInput ('Pin G2 _ ': rest) = 'Pin G2 ('InSpec 'PullUp 'Falling) ': AsInput rest
  AsInput ('Pin G3 _ ': rest) = 'Pin G3 ('InSpec 'PullUp 'Falling) ': AsInput rest
  AsInput ('Pin p _ ': rest) = 'Pin p ('InSpec 'PullDown 'Falling) ': AsInput rest

pinOffset :: Pin -> Int
pinOffset = fromEnum . pinGpio

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
  PullUpDown -> flagBiasPullUp .|. flagBiasPullDown

edgesFlag :: Edge -> GpioV2LineFlag
edgesFlag = \case
  Rising -> flagEdgeRising
  Falling -> flagEdgeFalling
  RisingAndFalling -> flagEdgeRising .|. flagEdgeFalling
    
directionFlags :: PinSpec -> [GpioV2LineFlag]
directionFlags = \case
  InSpec isBias isEdge  -> [ flagInput, biasFlag isBias, edgesFlag isEdge ]
  OutSpec _ -> [ flagOutput ]
