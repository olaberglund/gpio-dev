{-# LINE 1 "app/Gpio/Ioctl.hsc" #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Gpio.Ioctl where

import Foreign
import Foreign.C.Types
import Foreign.C.Error
import Data.ByteString qualified as BS
import Data.ByteString (ByteString)




newtype GpioV2LineFlag = GpioV2LineFlag { unGpioV2LineFlag :: Word64 }
  deriving (Eq, Bits)

attributeIdFlags :: Word32
attributeIdFlags = 1
{-# LINE 20 "app/Gpio/Ioctl.hsc" #-}

attributeIdValues :: Word32
attributeIdValues = 2
{-# LINE 23 "app/Gpio/Ioctl.hsc" #-}

-- https://docs.kernel.org/userspace-api/gpio/chardev.html#c.gpio_v2_line_flag
flagUsed  :: GpioV2LineFlag
flagUsed  = GpioV2LineFlag 1
flagActiveLow  :: GpioV2LineFlag
flagActiveLow  = GpioV2LineFlag 2
flagInput  :: GpioV2LineFlag
flagInput  = GpioV2LineFlag 4
flagOutput  :: GpioV2LineFlag
flagOutput  = GpioV2LineFlag 8
flagEdgeRising  :: GpioV2LineFlag
flagEdgeRising  = GpioV2LineFlag 16
flagEdgeFalling  :: GpioV2LineFlag
flagEdgeFalling  = GpioV2LineFlag 32
flagOpenDrain  :: GpioV2LineFlag
flagOpenDrain  = GpioV2LineFlag 64
flagOpenSource  :: GpioV2LineFlag
flagOpenSource  = GpioV2LineFlag 128
flagBiasPullUp  :: GpioV2LineFlag
flagBiasPullUp  = GpioV2LineFlag 256
flagBiasPullDown  :: GpioV2LineFlag
flagBiasPullDown  = GpioV2LineFlag 512
flagBiasDisabled  :: GpioV2LineFlag
flagBiasDisabled  = GpioV2LineFlag 1024
flagEventClockRealtime  :: GpioV2LineFlag
flagEventClockRealtime  = GpioV2LineFlag 2048
flagEventClockHte  :: GpioV2LineFlag
flagEventClockHte  = GpioV2LineFlag 4096

{-# LINE 40 "app/Gpio/Ioctl.hsc" #-}

data GpioV2LineAttribute = GpioV2LineAttribute
  { attributeId :: Word32
  , attributeFlags :: Word64 -- line flags added together
  }

instance Storable GpioV2LineAttribute where
  sizeOf _  = (16)
{-# LINE 48 "app/Gpio/Ioctl.hsc" #-}
  alignment _ = 8
{-# LINE 49 "app/Gpio/Ioctl.hsc" #-}
  peek p = do
    attributeId <- (\hsc_ptr -> peekByteOff hsc_ptr 0) p
{-# LINE 51 "app/Gpio/Ioctl.hsc" #-}
    attributeFlags <- (\hsc_ptr -> peekByteOff hsc_ptr 8) p
{-# LINE 52 "app/Gpio/Ioctl.hsc" #-}
    pure GpioV2LineAttribute {..}
  poke p GpioV2LineAttribute{..} = do
    fillBytes p 0 (16)
{-# LINE 55 "app/Gpio/Ioctl.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 0) p attributeId
{-# LINE 56 "app/Gpio/Ioctl.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 8) p attributeFlags
{-# LINE 57 "app/Gpio/Ioctl.hsc" #-}
      

data GpioV2LineConfigAttribute = GpioV2LineConfigAttribute
  { configAttr :: GpioV2LineAttribute
  , configMask :: Word64
  }

instance Storable GpioV2LineConfigAttribute where
  sizeOf _  = (24)
{-# LINE 66 "app/Gpio/Ioctl.hsc" #-}
  alignment _ = 8
{-# LINE 67 "app/Gpio/Ioctl.hsc" #-}
  peek p = do
    configAttr <- (\hsc_ptr -> peekByteOff hsc_ptr 0) p
{-# LINE 69 "app/Gpio/Ioctl.hsc" #-}
    configMask <- (\hsc_ptr -> peekByteOff hsc_ptr 16) p
{-# LINE 70 "app/Gpio/Ioctl.hsc" #-}
    pure GpioV2LineConfigAttribute {..}
  poke p GpioV2LineConfigAttribute{..} = do
    fillBytes p 0 (24)
{-# LINE 73 "app/Gpio/Ioctl.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 0) p configAttr
{-# LINE 74 "app/Gpio/Ioctl.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 16) p configMask
{-# LINE 75 "app/Gpio/Ioctl.hsc" #-}
      

emptyLineConfigAttribute :: GpioV2LineConfigAttribute
emptyLineConfigAttribute = GpioV2LineConfigAttribute
  { configAttr = GpioV2LineAttribute { attributeId = 0, attributeFlags = 0 }
  , configMask = 0
  }

data GpioV2LineConfig = GpioV2LineConfig
  { flags :: Word64
  , numAttrs :: Word32
  , attrs :: [GpioV2LineConfigAttribute]
  }

instance Storable GpioV2LineConfig where
  sizeOf _  = (272)
{-# LINE 91 "app/Gpio/Ioctl.hsc" #-}
  alignment _ = 8
{-# LINE 92 "app/Gpio/Ioctl.hsc" #-}
  peek p = do
    flags    <- (\hsc_ptr -> peekByteOff hsc_ptr 0) p
{-# LINE 94 "app/Gpio/Ioctl.hsc" #-}
    numAttrs <- (\hsc_ptr -> peekByteOff hsc_ptr 8) p
{-# LINE 95 "app/Gpio/Ioctl.hsc" #-}
    attrs    <- peekArray 10 ((\hsc_ptr -> hsc_ptr `plusPtr` 32) p)
{-# LINE 96 "app/Gpio/Ioctl.hsc" #-}
    pure GpioV2LineConfig {..}
  poke p GpioV2LineConfig{..} = do
    fillBytes p 0 (272)
{-# LINE 99 "app/Gpio/Ioctl.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 0) p flags
{-# LINE 100 "app/Gpio/Ioctl.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 8) p numAttrs
{-# LINE 101 "app/Gpio/Ioctl.hsc" #-}
    let padded = take 10 (attrs <> repeat emptyLineConfigAttribute)
{-# LINE 102 "app/Gpio/Ioctl.hsc" #-}
    pokeArray ((\hsc_ptr -> hsc_ptr `plusPtr` 32) p) padded
{-# LINE 103 "app/Gpio/Ioctl.hsc" #-}
      

data GpioV2LineRequest = GpioV2LineRequest
  { offsets         :: [Word32] 
  , consumer        :: ByteString
  , config          :: GpioV2LineConfig
  , numLines        :: Word32
  , eventBufferSize :: Word32
  , fileDescriptor  :: Int32
  }

instance Storable GpioV2LineRequest where
  sizeOf    _ = (592)
{-# LINE 116 "app/Gpio/Ioctl.hsc" #-}
  alignment _ = 8
{-# LINE 117 "app/Gpio/Ioctl.hsc" #-}

  peek p = do
    offsets         <- peekArray 64 ((\hsc_ptr -> hsc_ptr `plusPtr` 0) p)
{-# LINE 120 "app/Gpio/Ioctl.hsc" #-}
    consumer        <- BS.packCString ((\hsc_ptr -> hsc_ptr `plusPtr` 256) p)
{-# LINE 121 "app/Gpio/Ioctl.hsc" #-}
    config          <- peek ((\hsc_ptr -> hsc_ptr `plusPtr` 288) p)
{-# LINE 122 "app/Gpio/Ioctl.hsc" #-}
    numLines        <- (\hsc_ptr -> peekByteOff hsc_ptr 560) p
{-# LINE 123 "app/Gpio/Ioctl.hsc" #-}
    eventBufferSize <- (\hsc_ptr -> peekByteOff hsc_ptr 564) p
{-# LINE 124 "app/Gpio/Ioctl.hsc" #-}
    fileDescriptor  <- (\hsc_ptr -> peekByteOff hsc_ptr 588) p
{-# LINE 125 "app/Gpio/Ioctl.hsc" #-}
    pure (GpioV2LineRequest{..})

  poke p (GpioV2LineRequest offs cons cfg nl ebs fd) = do
    fillBytes p 0 (592)
{-# LINE 129 "app/Gpio/Ioctl.hsc" #-}
    let padded = take 64 (offs <> repeat 0)
{-# LINE 130 "app/Gpio/Ioctl.hsc" #-}
    pokeArray ((\hsc_ptr -> hsc_ptr `plusPtr` 0) p) padded
{-# LINE 131 "app/Gpio/Ioctl.hsc" #-}
    BS.useAsCStringLen (BS.take (32 - 1) cons) $ \(src, len) ->
{-# LINE 132 "app/Gpio/Ioctl.hsc" #-}
      copyBytes ((\hsc_ptr -> hsc_ptr `plusPtr` 256) p) src len
{-# LINE 133 "app/Gpio/Ioctl.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 288) p cfg
{-# LINE 134 "app/Gpio/Ioctl.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 560) p nl
{-# LINE 135 "app/Gpio/Ioctl.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 564) p ebs
{-# LINE 136 "app/Gpio/Ioctl.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 588) p fd
{-# LINE 137 "app/Gpio/Ioctl.hsc" #-}

data GpioV2LineValues = GpioV2LineValues
  { bits :: Word64
  , mask :: Word64
  }

instance Storable GpioV2LineValues where
    sizeOf _ = (16)
{-# LINE 145 "app/Gpio/Ioctl.hsc" #-}
    alignment _ = 8
{-# LINE 146 "app/Gpio/Ioctl.hsc" #-}
    peek p = do
        bits <- (\hsc_ptr -> peekByteOff hsc_ptr 0) p
{-# LINE 148 "app/Gpio/Ioctl.hsc" #-}
        mask <- (\hsc_ptr -> peekByteOff hsc_ptr 8) p
{-# LINE 149 "app/Gpio/Ioctl.hsc" #-}
        pure GpioV2LineValues{..}
    poke p GpioV2LineValues{..} = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) p bits
{-# LINE 152 "app/Gpio/Ioctl.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) p mask
{-# LINE 153 "app/Gpio/Ioctl.hsc" #-}

data GpioV2LineEvent = GpioV2LineEvent
  { timestamp_ns :: Word64
  , id :: Word32
  , offset :: Word32
  , seqno :: Word32
  , line_seqno :: Word32
  }

newtype GpioV2LineEventId = GpioV2LineEventId { unGpioV2LineEventId :: Word32 }

risingEdge  :: GpioV2LineEventId
risingEdge  = GpioV2LineEventId 1
fallingEdge  :: GpioV2LineEventId
fallingEdge  = GpioV2LineEventId 2

{-# LINE 168 "app/Gpio/Ioctl.hsc" #-}

instance Storable GpioV2LineEvent where
    sizeOf _ = (48)
{-# LINE 171 "app/Gpio/Ioctl.hsc" #-}
    alignment _ = 8
{-# LINE 172 "app/Gpio/Ioctl.hsc" #-}
    peek p = do
        timestamp_ns <- (\hsc_ptr -> peekByteOff hsc_ptr 0) p
{-# LINE 174 "app/Gpio/Ioctl.hsc" #-}
        id <- (\hsc_ptr -> peekByteOff hsc_ptr 8) p
{-# LINE 175 "app/Gpio/Ioctl.hsc" #-}
        offset <- (\hsc_ptr -> peekByteOff hsc_ptr 12) p
{-# LINE 176 "app/Gpio/Ioctl.hsc" #-}
        seqno <- (\hsc_ptr -> peekByteOff hsc_ptr 16) p
{-# LINE 177 "app/Gpio/Ioctl.hsc" #-}
        line_seqno <- (\hsc_ptr -> peekByteOff hsc_ptr 20) p
{-# LINE 178 "app/Gpio/Ioctl.hsc" #-}
        pure GpioV2LineEvent{..}
    poke p GpioV2LineEvent{..} = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) p timestamp_ns
{-# LINE 181 "app/Gpio/Ioctl.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) p id
{-# LINE 182 "app/Gpio/Ioctl.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 12) p offset
{-# LINE 183 "app/Gpio/Ioctl.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 16) p seqno
{-# LINE 184 "app/Gpio/Ioctl.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 20) p line_seqno
{-# LINE 185 "app/Gpio/Ioctl.hsc" #-}

foreign import ccall safe "sys/ioctl.h ioctl"
  c_ioctl :: CInt -> CULong -> Ptr a -> IO CInt

requestLines :: CInt -> Ptr GpioV2LineRequest -> IO ()
requestLines fd p =
  throwErrnoIfMinus1_ "ioctl GPIO_V2_GET_LINE"
    (c_ioctl (fromIntegral fd) 3260068871 p)
{-# LINE 193 "app/Gpio/Ioctl.hsc" #-}

setValues :: CInt -> Ptr GpioV2LineValues -> IO ()
setValues fd p = throwErrnoIfMinus1_ "ioctl GPIO_V2_LINE_SET_VALUES_IOCTL" 
    (c_ioctl (fromIntegral fd) 3222320143 p)
{-# LINE 197 "app/Gpio/Ioctl.hsc" #-}

getValues :: CInt -> Ptr GpioV2LineValues -> IO ()
getValues fd p = throwErrnoIfMinus1_ "ioctl GPIO_V2_LINE_GET_VALUES_IOCTL" 
    (c_ioctl (fromIntegral fd) 3222320142 p)
{-# LINE 201 "app/Gpio/Ioctl.hsc" #-}

setLineConfigs :: CInt -> Ptr GpioV2LineConfig -> IO ()
setLineConfigs fd p = throwErrnoIfMinus1_ "ioctl GPIO_V2_LINE_SET_CONFIG_IOCTL" 
    (c_ioctl (fromIntegral fd) 3239097357 p)
{-# LINE 205 "app/Gpio/Ioctl.hsc" #-}

foreign import ccall safe "unistd.h read"
    c_read :: CInt -> Ptr a -> CSize -> IO CInt

readEvents :: CInt -> CSize -> Ptr GpioV2LineEvent -> IO CInt
readEvents fd sz buf = throwErrnoIfMinus1 "read gpio event" (c_read (fromIntegral fd) buf sz)
