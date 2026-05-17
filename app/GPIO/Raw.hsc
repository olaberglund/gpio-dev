{-# LANGUAGE RecordWildCards #-}

module GPIO.Raw where

import Foreign
import Foreign.C.Types
import Foreign.C.Error
import Foreign.C.String
import Data.ByteString qualified as BS
import Data.ByteString (ByteString)

#include <linux/gpio.h>
#include <fcntl.h>

output :: Word64
output = #const GPIO_V2_LINE_FLAG_OUTPUT

data GpioV2LineConfig = GpioV2LineConfig
  { flags :: Word64
  , numAttrs :: Word32
  -- TODO: gpio_v2_line_config_attribute
  }

instance Storable GpioV2LineConfig where
  sizeOf _  = #size struct gpio_v2_line_config
  alignment _ = #alignment struct gpio_v2_line_config
  peek p = do
    flags    <- #{peek struct gpio_v2_line_config, flags} p
    numAttrs <- #{peek struct gpio_v2_line_config, num_attrs} p
    pure GpioV2LineConfig {..}
  poke p GpioV2LineConfig{..} = do
    fillBytes p 0 #{size struct gpio_v2_line_config}
    #{poke struct gpio_v2_line_config, flags} p flags
    #{poke struct gpio_v2_line_config, num_attrs} p numAttrs
      

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

foreign import ccall safe "sys/ioctl.h ioctl"
  c_ioctl :: CInt -> CULong -> Ptr a -> IO CInt

requestLines :: CInt -> Ptr GpioV2LineRequest -> IO ()
requestLines fd p =
  throwErrnoIfMinus1_ "ioctl GPIO_V2_GET_LINE"
    (c_ioctl (fromIntegral fd) #{const GPIO_V2_GET_LINE_IOCTL} p)

setValues :: CInt -> Ptr GpioV2LineValues -> IO ()
setValues fd p = throwErrnoIfMinus1_ "ioctl GPIO_V2_GET_LINE" 
    (c_ioctl (fromIntegral fd) #{const GPIO_V2_LINE_SET_VALUES_IOCTL} p)
