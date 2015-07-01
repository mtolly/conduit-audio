{-# LANGUAGE NondecreasingIndentation #-}
module Data.Conduit.Audio.AO.Binding where

import Foreign
import Foreign.C
import Data.List (intercalate)

#include <ao/ao.h>

data Device
{#pointer *ao_device as Device_ -> `Device' #}

newtype Option = Option [(String, String)]
{#pointer *ao_option as Option_ -> `Option' #}

data SampleFormat = SampleFormat
  { bits :: CInt
  , rate :: CInt
  , channels :: CInt
  , byteFormat :: Format
  , matrix :: Maybe [Channel]
  } deriving (Eq, Ord, Show)
{#pointer *ao_sample_format as SampleFormat_ -> `SampleFormat' #}

{#enum define Format
  { AO_FMT_LITTLE as AO_FMT_LITTLE
  , AO_FMT_BIG    as AO_FMT_BIG
  , AO_FMT_NATIVE as AO_FMT_NATIVE
  } deriving (Eq, Ord, Show, Read, Bounded) #}

data Channel
  = L -- ^ Left speaker, located forward and to the left of the listener.
  | R -- ^ Right speaker, located forward and to the right of the listener.
  | C -- ^ Center speaker, located directly forward of the listener between the Left and Right speakers.
  | M -- ^ Monophonic, a virtual speaker for single-channel output.
  | CL -- ^ Left of Center speaker (used in some Widescreen formats), located forward of the listener between the Center and Left speakers. Alternatively referred to as \'Left Center\'.
  | CR -- ^ Right of Center speaker (used in some Widescreen formats), located forward of the listener between the Center and Right speakers. Alternatively referred to as \'Right Center\'.
  | BL -- ^ Back Left speaker, located behind and to the left of the listener. Alternatively called \'Left Surround\' (primarily by Apple) or \'Surround Rear Left\' (primarily by Dolby).
  | BR -- ^ Back Right speaker, located behind and to the right of the listener. Alternatively called \'Right Surround\' (primarily by Apple) or \'Surround Rear Right\' (primarily by Dolby).
  | BC -- ^ Back Center speaker, located directly behind the listener. Alternatively called \'Center Surround\' (primarily by Apple) or \'Surround Rear Center\' (primarily by Dolby).
  | SL -- ^ Side Left speaker, located directly to the listener\'s left side. The Side Left speaker is also referred to as \'Left Surround Direct\' (primarily by Apple) or \'Surround Left\' (primarily by Dolby)
  | SR -- ^ Side Right speaker, located directly to the listener\'s right side. The Side Right speaker is also referred to as \'Right Surround Direct\' (primarily by Apple) or \'Surround Right\' (primarily by Dolby)
  | LFE -- ^ Low Frequency Effect (subwoofer) channel. This is channel is usually lowpassed and meant only for bass, though in some recent formats it is a discrete, full-range channel. Microsoft calls this the \'Low Frequency\' channel.
  | A1 -- ^ \'auxiliary\' channels, not mapped to a location. Intended for driver-specific use.
  | A2 -- ^ \'auxiliary\' channels, not mapped to a location. Intended for driver-specific use.
  | A3 -- ^ \'auxiliary\' channels, not mapped to a location. Intended for driver-specific use.
  | A4 -- ^ \'auxiliary\' channels, not mapped to a location. Intended for driver-specific use.
  | X -- ^ Unused/Invalid channel, to be dropped in the driver and not output to any speaker.
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

withOption :: Option -> (Option_ -> IO a) -> IO a
withOption (Option dict) act = go nullPtr $ reverse dict where
  go p [] = act p
  go p ((k, v) : rest) = do
    allocaBytes {#sizeof ao_option #} $ \p' -> do
    withCString k $ \pk -> do
    withCString v $ \pv -> do
    {#set ao_option.key   #} p' pk
    {#set ao_option.value #} p' pv
    {#set ao_option.next  #} p' p
    go p' rest

withSampleFormat :: SampleFormat -> (SampleFormat_ -> IO a) -> IO a
withSampleFormat fmt act = do
  allocaBytes {#sizeof ao_sample_format #} $ \p -> do
  {#set ao_sample_format.bits #} p $ bits fmt
  {#set ao_sample_format.rate #} p $ rate fmt
  {#set ao_sample_format.channels #} p $ channels fmt
  {#set ao_sample_format.byte_format #} p $ fromIntegral $ fromEnum $ byteFormat fmt
  (case matrix fmt of
    Nothing -> ($ nullPtr)
    Just xs -> withCString (intercalate "," $ map show xs)
    ) $ \s -> do
    {#set ao_sample_format.matrix #} p s
    act p

{#context prefix="ao_"#}

{#fun initialize as ^ {} -> `()' #}
{#fun shutdown as ^ {} -> `()' #}
{#fun default_driver_id as defaultDriverID {} -> `CInt' #}
{#fun open_live as ^ { `CInt', `SampleFormat_', `Option_' } -> `Device_' #}
{#fun play as ^ { `Device_', castPtr `Ptr Word8', `Int32' } -> `CInt' #}
{#fun close as ^ { `Device_' } -> `CInt' #}
