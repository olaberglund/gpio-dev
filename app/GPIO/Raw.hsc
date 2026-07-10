{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module GPIO.Raw where

import Foreign
import Foreign.C.Types
import Foreign.C.Error
import Data.ByteString qualified as BS
import Data.ByteString (ByteString)

#include <linux/gpio.h>
#include <fcntl.h>

newtype GpioV2LineFlag = GpioV2LineFlag { unGpioV2LineFlag :: Word64 }
  deriving (Eq, Bits)

attributeIdFlags :: Word32
attributeIdFlags = #{const GPIO_V2_LINE_ATTR_ID_FLAGS}

attributeIdValues :: Word32
attributeIdValues = #{const GPIO_V2_LINE_ATTR_ID_OUTPUT_VALUES}

-- https://docs.kernel.org/userspace-api/gpio/chardev.html#c.gpio_v2_line_flag
#{enum GpioV2LineFlag, GpioV2LineFlag
  , flagUsed = GPIO_V2_LINE_FLAG_USED
  , flagActiveLow = GPIO_V2_LINE_FLAG_ACTIVE_LOW
  , flagInput = GPIO_V2_LINE_FLAG_INPUT
  , flagOutput = GPIO_V2_LINE_FLAG_OUTPUT
  , flagEdgeRising = GPIO_V2_LINE_FLAG_EDGE_RISING
  , flagEdgeFalling = GPIO_V2_LINE_FLAG_EDGE_FALLING
  , flagOpenDrain = GPIO_V2_LINE_FLAG_OPEN_DRAIN
  , flagOpenSource = GPIO_V2_LINE_FLAG_OPEN_SOURCE
  , flagBiasPullUp = GPIO_V2_LINE_FLAG_BIAS_PULL_UP
  , flagBiasPullDown = GPIO_V2_LINE_FLAG_BIAS_PULL_DOWN
  , flagBiasDisabled = GPIO_V2_LINE_FLAG_BIAS_DISABLED
  , flagEventClockRealtime = GPIO_V2_LINE_FLAG_EVENT_CLOCK_REALTIME
  , flagEventClockHte = GPIO_V2_LINE_FLAG_EVENT_CLOCK_HTE
}

data GpioV2LineAttribute = GpioV2LineAttribute
  { attributeId :: Word32
  , attributeFlags :: Word64 -- line flags added together
  }

instance Storable GpioV2LineAttribute where
  sizeOf _  = #size struct gpio_v2_line_attribute
  alignment _ = #alignment struct gpio_v2_line_attribute
  peek p = do
    attributeId <- #{peek struct gpio_v2_line_attribute, id} p
    attributeFlags <- #{peek struct gpio_v2_line_attribute, flags} p
    pure GpioV2LineAttribute {..}
  poke p GpioV2LineAttribute{..} = do
    fillBytes p 0 #{size struct gpio_v2_line_attribute}
    #{poke struct gpio_v2_line_attribute, id} p attributeId
    #{poke struct gpio_v2_line_attribute, flags} p attributeFlags
      

data GpioV2LineConfigAttribute = GpioV2LineConfigAttribute
  { configAttr :: GpioV2LineAttribute
  , configMask :: Word64
  }

instance Storable GpioV2LineConfigAttribute where
  sizeOf _  = #size struct gpio_v2_line_config_attribute
  alignment _ = #alignment struct gpio_v2_line_config_attribute
  peek p = do
    configAttr <- #{peek struct gpio_v2_line_config_attribute, attr} p
    configMask <- #{peek struct gpio_v2_line_config_attribute, mask} p
    pure GpioV2LineConfigAttribute {..}
  poke p GpioV2LineConfigAttribute{..} = do
    fillBytes p 0 #{size struct gpio_v2_line_config_attribute}
    #{poke struct gpio_v2_line_config_attribute, attr} p configAttr
    #{poke struct gpio_v2_line_config_attribute, mask} p configMask
      

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
  sizeOf _  = #size struct gpio_v2_line_config
  alignment _ = #alignment struct gpio_v2_line_config
  peek p = do
    flags    <- #{peek struct gpio_v2_line_config, flags} p
    numAttrs <- #{peek struct gpio_v2_line_config, num_attrs} p
    attrs    <- peekArray #{const GPIO_V2_LINE_NUM_ATTRS_MAX} (#{ptr struct gpio_v2_line_config, attrs} p)
    pure GpioV2LineConfig {..}
  poke p GpioV2LineConfig{..} = do
    fillBytes p 0 #{size struct gpio_v2_line_config}
    #{poke struct gpio_v2_line_config, flags} p flags
    #{poke struct gpio_v2_line_config, num_attrs} p numAttrs
    let padded = take #{const GPIO_V2_LINE_NUM_ATTRS_MAX} (attrs <> repeat emptyLineConfigAttribute)
    pokeArray (#{ptr struct gpio_v2_line_config, attrs} p) padded
      

data GpioV2LineRequest = GpioV2LineRequest
  { offsets         :: [Word32] 
  , consumer        :: ByteString
  , config          :: GpioV2LineConfig
  , numLines        :: Word32
  , eventBufferSize :: Word32
  , fileDescriptor  :: Int32
  }

instance Storable GpioV2LineRequest where
  sizeOf    _ = #size struct gpio_v2_line_request
  alignment _ = #alignment struct gpio_v2_line_request

  peek p = do
    offsets         <- peekArray #{const GPIO_V2_LINES_MAX} (#{ptr struct gpio_v2_line_request, offsets} p)
    consumer        <- BS.packCString (#{ptr struct gpio_v2_line_request, consumer} p)
    config          <- peek (#{ptr struct gpio_v2_line_request, config} p)
    numLines        <- #{peek struct gpio_v2_line_request, num_lines} p
    eventBufferSize <- #{peek struct gpio_v2_line_request, event_buffer_size} p
    fileDescriptor  <- #{peek struct gpio_v2_line_request, fd} p
    pure (GpioV2LineRequest{..})

  poke p (GpioV2LineRequest offs cons cfg nl ebs fd) = do
    fillBytes p 0 #{size struct gpio_v2_line_request}
    let padded = take #{const GPIO_V2_LINES_MAX} (offs <> repeat 0)
    pokeArray (#{ptr struct gpio_v2_line_request, offsets} p) padded
    BS.useAsCStringLen (BS.take (#{const GPIO_MAX_NAME_SIZE} - 1) cons) $ \(src, len) ->
      copyBytes (#{ptr struct gpio_v2_line_request, consumer} p) src len
    #{poke struct gpio_v2_line_request, config} p cfg
    #{poke struct gpio_v2_line_request, num_lines} p nl
    #{poke struct gpio_v2_line_request, event_buffer_size} p ebs
    #{poke struct gpio_v2_line_request, fd} p fd

data GpioV2LineValues = GpioV2LineValues
  { bits :: Word64
  , mask :: Word64
  }

instance Storable GpioV2LineValues where
    sizeOf _ = #size struct gpio_v2_line_values
    alignment _ = #alignment struct gpio_v2_line_values
    peek p = do
        bits <- #{peek struct gpio_v2_line_values, bits} p
        mask <- #{peek struct gpio_v2_line_values, mask} p
        pure GpioV2LineValues{..}
    poke p GpioV2LineValues{..} = do
        #{poke struct gpio_v2_line_values, bits} p bits
        #{poke struct gpio_v2_line_values, mask} p mask

data GpioV2LineEvent = GpioV2LineEvent
  { timestamp_ns :: Word64
  , id :: Word32
  , offset :: Word32
  , seqno :: Word32
  , line_seqno :: Word32
  }

newtype GpioV2LineEventId = GpioV2LineEventId { unGpioV2LineEventId :: Word32 }

#{enum GpioV2LineEventId, GpioV2LineEventId
  , risingEdge = GPIO_V2_LINE_EVENT_RISING_EDGE
  , fallingEdge = GPIO_V2_LINE_EVENT_FALLING_EDGE
}

instance Storable GpioV2LineEvent where
    sizeOf _ = #size struct gpio_v2_line_event
    alignment _ = #alignment struct gpio_v2_line_event
    peek p = do
        timestamp_ns <- #{peek struct gpio_v2_line_event, timestamp_ns} p
        id <- #{peek struct gpio_v2_line_event, id} p
        offset <- #{peek struct gpio_v2_line_event, offset} p
        seqno <- #{peek struct gpio_v2_line_event, seqno} p
        line_seqno <- #{peek struct gpio_v2_line_event, line_seqno} p
        pure GpioV2LineEvent{..}
    poke p GpioV2LineEvent{..} = do
        #{poke struct gpio_v2_line_event, timestamp_ns} p timestamp_ns
        #{poke struct gpio_v2_line_event, id} p id
        #{poke struct gpio_v2_line_event, offset} p offset
        #{poke struct gpio_v2_line_event, seqno} p seqno
        #{poke struct gpio_v2_line_event, line_seqno} p line_seqno

foreign import ccall safe "sys/ioctl.h ioctl"
  c_ioctl :: CInt -> CULong -> Ptr a -> IO CInt

requestLines :: CInt -> Ptr GpioV2LineRequest -> IO ()
requestLines fd p =
  throwErrnoIfMinus1_ "ioctl GPIO_V2_GET_LINE"
    (c_ioctl (fromIntegral fd) #{const GPIO_V2_GET_LINE_IOCTL} p)

setValues :: CInt -> Ptr GpioV2LineValues -> IO ()
setValues fd p = throwErrnoIfMinus1_ "ioctl GPIO_V2_LINE_SET_VALUES_IOCTL" 
    (c_ioctl (fromIntegral fd) #{const GPIO_V2_LINE_SET_VALUES_IOCTL} p)

getValues :: CInt -> Ptr GpioV2LineValues -> IO ()
getValues fd p = throwErrnoIfMinus1_ "ioctl GPIO_V2_LINE_GET_VALUES_IOCTL" 
    (c_ioctl (fromIntegral fd) #{const GPIO_V2_LINE_GET_VALUES_IOCTL} p)

setLineConfigs :: CInt -> Ptr GpioV2LineConfig -> IO ()
setLineConfigs fd p = throwErrnoIfMinus1_ "ioctl GPIO_V2_LINE_SET_CONFIG_IOCTL" 
    (c_ioctl (fromIntegral fd) #{const GPIO_V2_LINE_SET_CONFIG_IOCTL} p)

foreign import ccall safe "unistd.h read"
    c_read :: CInt -> Ptr a -> CSize -> IO CInt

readEvents :: CInt -> CSize -> Ptr GpioV2LineEvent -> IO CInt
readEvents fd sz buf = throwErrnoIfMinus1 "read gpio event" (c_read (fromIntegral fd) buf sz)
