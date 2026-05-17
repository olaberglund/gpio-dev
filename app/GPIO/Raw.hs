{-# LINE 1 "app/GPIO/Raw.hsc" #-}
{-# LANGUAGE RecordWildCards #-}

module GPIO.Raw where

import Foreign
import Foreign.C.Types
import Foreign.C.Error
import Foreign.C.String
import Data.ByteString qualified as BS
import Data.ByteString (ByteString)




output :: Word64
output = 8
{-# LINE 17 "app/GPIO/Raw.hsc" #-}

data GpioV2LineConfig = GpioV2LineConfig
  { flags :: Word64
  , numAttrs :: Word32
  -- TODO: gpio_v2_line_config_attribute
  }

instance Storable GpioV2LineConfig where
  sizeOf _  = (272)
{-# LINE 26 "app/GPIO/Raw.hsc" #-}
  alignment _ = 8
{-# LINE 27 "app/GPIO/Raw.hsc" #-}
  peek p = do
    flags    <- (\hsc_ptr -> peekByteOff hsc_ptr 0) p
{-# LINE 29 "app/GPIO/Raw.hsc" #-}
    numAttrs <- (\hsc_ptr -> peekByteOff hsc_ptr 8) p
{-# LINE 30 "app/GPIO/Raw.hsc" #-}
    pure GpioV2LineConfig {..}
  poke p GpioV2LineConfig{..} = do
    fillBytes p 0 (272)
{-# LINE 33 "app/GPIO/Raw.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 0) p flags
{-# LINE 34 "app/GPIO/Raw.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 8) p numAttrs
{-# LINE 35 "app/GPIO/Raw.hsc" #-}
      

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
{-# LINE 48 "app/GPIO/Raw.hsc" #-}
  alignment _ = 8
{-# LINE 49 "app/GPIO/Raw.hsc" #-}

  peek p = do
    offsets         <- peekArray 64 ((\hsc_ptr -> hsc_ptr `plusPtr` 0) p)
{-# LINE 52 "app/GPIO/Raw.hsc" #-}
    consumer        <- BS.packCString ((\hsc_ptr -> hsc_ptr `plusPtr` 256) p)
{-# LINE 53 "app/GPIO/Raw.hsc" #-}
    config          <- peek ((\hsc_ptr -> hsc_ptr `plusPtr` 288) p)
{-# LINE 54 "app/GPIO/Raw.hsc" #-}
    numLines        <- (\hsc_ptr -> peekByteOff hsc_ptr 560) p
{-# LINE 55 "app/GPIO/Raw.hsc" #-}
    eventBufferSize <- (\hsc_ptr -> peekByteOff hsc_ptr 564) p
{-# LINE 56 "app/GPIO/Raw.hsc" #-}
    fileDescriptor  <- (\hsc_ptr -> peekByteOff hsc_ptr 588) p
{-# LINE 57 "app/GPIO/Raw.hsc" #-}
    pure (GpioV2LineRequest{..})

  poke p (GpioV2LineRequest offs cons cfg nl ebs fd) = do
    fillBytes p 0 (592)
{-# LINE 61 "app/GPIO/Raw.hsc" #-}
    let padded = take 64 (offs <> repeat 0)
{-# LINE 62 "app/GPIO/Raw.hsc" #-}
    pokeArray ((\hsc_ptr -> hsc_ptr `plusPtr` 0) p) padded
{-# LINE 63 "app/GPIO/Raw.hsc" #-}
    BS.useAsCStringLen (BS.take (32 - 1) cons) $ \(src, len) ->
{-# LINE 64 "app/GPIO/Raw.hsc" #-}
      copyBytes ((\hsc_ptr -> hsc_ptr `plusPtr` 256) p) src len
{-# LINE 65 "app/GPIO/Raw.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 288) p cfg
{-# LINE 66 "app/GPIO/Raw.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 560) p nl
{-# LINE 67 "app/GPIO/Raw.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 564) p ebs
{-# LINE 68 "app/GPIO/Raw.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 588) p fd
{-# LINE 69 "app/GPIO/Raw.hsc" #-}

data GpioV2LineValues = GpioV2LineValues
  { bits :: Word64
  , mask :: Word64
  }

instance Storable GpioV2LineValues where
    sizeOf _ = (16)
{-# LINE 77 "app/GPIO/Raw.hsc" #-}
    alignment _ = 8
{-# LINE 78 "app/GPIO/Raw.hsc" #-}
    peek p = do
        bits <- (\hsc_ptr -> peekByteOff hsc_ptr 0) p
{-# LINE 80 "app/GPIO/Raw.hsc" #-}
        mask <- (\hsc_ptr -> peekByteOff hsc_ptr 8) p
{-# LINE 81 "app/GPIO/Raw.hsc" #-}
        pure GpioV2LineValues{..}
    poke p GpioV2LineValues{..} = do
        (\hsc_ptr -> pokeByteOff hsc_ptr 0) p bits
{-# LINE 84 "app/GPIO/Raw.hsc" #-}
        (\hsc_ptr -> pokeByteOff hsc_ptr 8) p mask
{-# LINE 85 "app/GPIO/Raw.hsc" #-}

foreign import ccall safe "sys/ioctl.h ioctl"
  c_ioctl :: CInt -> CULong -> Ptr a -> IO CInt

requestLines :: CInt -> Ptr GpioV2LineRequest -> IO ()
requestLines fd p =
  throwErrnoIfMinus1_ "ioctl GPIO_V2_GET_LINE"
    (c_ioctl (fromIntegral fd) 3260068871 p)
{-# LINE 93 "app/GPIO/Raw.hsc" #-}

setValues :: CInt -> Ptr GpioV2LineValues -> IO ()
setValues fd p = throwErrnoIfMinus1_ "ioctl GPIO_V2_GET_LINE" 
    (c_ioctl (fromIntegral fd) 3222320143 p)
{-# LINE 97 "app/GPIO/Raw.hsc" #-}
