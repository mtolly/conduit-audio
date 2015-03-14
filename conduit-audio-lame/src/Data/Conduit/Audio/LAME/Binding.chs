module Data.Conduit.Audio.LAME.Binding where

import Foreign
import Foreign.C

#include <lame/lame.h>

{#pointer *lame_global_flags as LAME newtype #}

{#context prefix="lame_"#}

{#fun init as ^ {} -> `LAME' #}



{#fun get_num_samples as ^ { `LAME' } -> `Int'  #}
{#fun set_num_samples as ^ { `LAME',  `Int' } -> `Int' #}
{#fun get_in_samplerate as ^ { `LAME' } -> `Int'  #}
{#fun set_in_samplerate as ^ { `LAME',  `Int' } -> `Int' #}
{#fun get_num_channels as ^ { `LAME' } -> `Int'  #}
{#fun set_num_channels as ^ { `LAME',  `Int' } -> `Int' #}
{#fun get_scale as ^ { `LAME' } -> `Float'  #}
{#fun set_scale as ^ { `LAME',  `Float' } -> `Int' #}
{#fun get_scale_left as ^ { `LAME' } -> `Float'  #}
{#fun set_scale_left as ^ { `LAME',  `Float' } -> `Int' #}
{#fun get_scale_right as ^ { `LAME' } -> `Float'  #}
{#fun set_scale_right as ^ { `LAME',  `Float' } -> `Int' #}
{#fun get_out_samplerate as ^ { `LAME' } -> `Int'  #}
{#fun set_out_samplerate as ^ { `LAME',  `Int' } -> `Int' #}
{#fun get_analysis as ^ { `LAME' } -> `Bool'  #}
{#fun set_analysis as ^ { `LAME',  `Bool' } -> `Int' #}
{#fun get_bWriteVbrTag as ^ { `LAME' } -> `Bool'  #}
{#fun set_bWriteVbrTag as ^ { `LAME',  `Bool' } -> `Int' #}
{#fun get_decode_only as ^ { `LAME' } -> `Bool'  #}
{#fun set_decode_only as ^ { `LAME',  `Bool' } -> `Int' #}
{#fun get_quality as ^ { `LAME' } -> `Int'  #}
{#fun set_quality as ^ { `LAME',  `Int' } -> `Int' #}
{#fun get_mode as ^ { `LAME' } -> `MPEGMode' toCEnum #}
{#fun set_mode as ^ { `LAME', fromCEnum `MPEGMode' } -> `Int' #}

{#enum MPEG_mode as MPEGMode {underscoreToCase}
  deriving (Eq, Ord, Show, Read, Bounded) #}

fromCEnum :: (Enum a) => a -> CInt
fromCEnum = fromIntegral . fromEnum

toCEnum :: (Enum a) => CInt -> a
toCEnum = toEnum . fromIntegral

{#fun get_force_ms as ^ { `LAME' } -> `Bool'  #}
{#fun set_force_ms as ^ { `LAME',  `Bool' } -> `Int' #}
{#fun get_free_format as ^ { `LAME' } -> `Bool'  #}
{#fun set_free_format as ^ { `LAME',  `Bool' } -> `Int' #}
{#fun get_findReplayGain as ^ { `LAME' } -> `Bool'  #}
{#fun set_findReplayGain as ^ { `LAME',  `Bool' } -> `Int' #}
{#fun get_decode_on_the_fly as ^ { `LAME' } -> `Bool'  #}
{#fun set_decode_on_the_fly as ^ { `LAME',  `Bool' } -> `Int' #}
{#fun get_nogap_total as ^ { `LAME' } -> `Int'  #}
{#fun set_nogap_total as ^ { `LAME',  `Int' } -> `Int' #}
{#fun get_nogap_currentindex as ^ { `LAME' } -> `Int'  #}
{#fun set_nogap_currentindex as ^ { `LAME',  `Int' } -> `Int' #}

-- TODO: lame_set_errorf, lame_set_debugf, lame_set_msgf

{#fun get_brate as ^ { `LAME' } -> `Int'  #}
{#fun set_brate as ^ { `LAME',  `Int' } -> `Int' #}
{#fun get_compression_ratio as ^ { `LAME' } -> `Float'  #}
{#fun set_compression_ratio as ^ { `LAME',  `Float' } -> `Int' #}

{#fun set_preset as ^ { `LAME', `Int' } -> `Int' #}
{#fun set_asm_optimizations as ^ { `LAME', `Int', `Int' } -> `Int' #}

{#fun get_copyright as ^ { `LAME' } -> `Bool'  #}
{#fun set_copyright as ^ { `LAME',  `Bool' } -> `Int' #}
{#fun get_original as ^ { `LAME' } -> `Bool'  #}
{#fun set_original as ^ { `LAME',  `Bool' } -> `Int' #}
{#fun get_error_protection as ^ { `LAME' } -> `Bool'  #}
{#fun set_error_protection as ^ { `LAME',  `Bool' } -> `Int' #}
{#fun get_extension as ^ { `LAME' } -> `Bool'  #}
{#fun set_extension as ^ { `LAME',  `Bool' } -> `Int' #}
{#fun get_strict_ISO as ^ { `LAME' } -> `Bool'  #}
{#fun set_strict_ISO as ^ { `LAME',  `Bool' } -> `Int' #}

-- TODO: quantization/noise shaping section

{#fun get_VBR as ^ { `LAME' } -> `VBRMode' toCEnum #}
{#fun set_VBR as ^ { `LAME', fromCEnum `VBRMode' } -> `Int' #}

{#enum vbr_mode as VBRMode {underscoreToCase}
  deriving (Eq, Ord, Show, Read, Bounded) #}

{#fun get_VBR_q as ^ { `LAME' } -> `Int'  #}
{#fun set_VBR_q as ^ { `LAME',  `Int' } -> `Int' #}
{#fun get_VBR_quality as ^ { `LAME' } -> `Float'  #}
{#fun set_VBR_quality as ^ { `LAME',  `Float' } -> `Int' #}

{#fun get_VBR_mean_bitrate_kbps as ^ { `LAME' } -> `Int'  #}
{#fun set_VBR_mean_bitrate_kbps as ^ { `LAME',  `Int' } -> `Int' #}
{#fun get_VBR_min_bitrate_kbps as ^ { `LAME' } -> `Int'  #}
{#fun set_VBR_min_bitrate_kbps as ^ { `LAME',  `Int' } -> `Int' #}
{#fun get_VBR_max_bitrate_kbps as ^ { `LAME' } -> `Int'  #}
{#fun set_VBR_max_bitrate_kbps as ^ { `LAME',  `Int' } -> `Int' #}

{#fun get_VBR_hard_min as ^ { `LAME' } -> `Bool'  #}
{#fun set_VBR_hard_min as ^ { `LAME',  `Bool' } -> `Int' #}

{#fun get_lowpassfreq as ^ { `LAME' } -> `Int'  #}
{#fun set_lowpassfreq as ^ { `LAME',  `Int' } -> `Int' #}
{#fun get_lowpasswidth as ^ { `LAME' } -> `Int'  #}
{#fun set_lowpasswidth as ^ { `LAME',  `Int' } -> `Int' #}
{#fun get_highpassfreq as ^ { `LAME' } -> `Int'  #}
{#fun set_highpassfreq as ^ { `LAME',  `Int' } -> `Int' #}
{#fun get_highpasswidth as ^ { `LAME' } -> `Int'  #}
{#fun set_highpasswidth as ^ { `LAME',  `Int' } -> `Int' #}

-- TODO: psycho acoustics section

{#fun get_version as ^ { `LAME' } -> `Int'  #}
{#fun get_encoder_delay as ^ { `LAME' } -> `Int'  #}
{#fun get_encoder_padding as ^ { `LAME' } -> `Int'  #}
{#fun get_framesize as ^ { `LAME' } -> `Int'  #}
{#fun get_mf_samples_to_encode as ^ { `LAME' } -> `Int'  #}
{#fun get_size_mp3buffer as ^ { `LAME' } -> `Int'  #}
{#fun get_frameNum as ^ { `LAME' } -> `Int'  #}
{#fun get_totalframes as ^ { `LAME' } -> `Int'  #}
{#fun get_RadioGain as ^ { `LAME' } -> `Int'  #}
{#fun get_AudiophileGain as ^ { `LAME' } -> `Int'  #}
{#fun get_PeakSample as ^ { `LAME' } -> `Float'  #}
{#fun get_noclipGainChange as ^ { `LAME' } -> `Int'  #}
{#fun get_noclipScale as ^ { `LAME' } -> `Float'  #}

{#fun init_params as ^ { `LAME' } -> `Int' #}


{#fun get_lame_version as ^ {} -> `String' #}

{#fun get_lame_short_version as ^ {} -> `String' #}

{#fun get_lame_very_short_version as ^ {} -> `String' #}

{#fun get_psy_version as ^ {} -> `String' #}

{#fun get_lame_url as ^ {} -> `String' #}

{#fun get_lame_os_bitness as ^ {} -> `String' #}


-- TODO: get_lame_version_numerical

{#fun print_config as ^ { `LAME' } -> `()' #}
{#fun print_internals as ^ { `LAME' } -> `()' #}






{#fun encode_buffer as ^
  { `LAME'
  , id `Ptr CShort' -- ^ PCM data for left channel
  , id `Ptr CShort' -- ^ PCM data for right channel
  , `Int' -- ^ number of samples per channel
  , id `Ptr CUChar' -- ^ pointer to encoded MP3 stream
  , `Int' -- ^ number of valid octets in this stream
  } -> `Int' #}


{#fun encode_buffer_interleaved as ^
  { `LAME'
  , id `Ptr CShort' -- ^ PCM data for left and right channel, interleaved
  , `Int' -- ^ number of samples per channel, /not/ number of samples overall
  , id `Ptr CUChar' -- ^ pointer to encoded MP3 stream
  , `Int' -- ^ number of valid octets in this stream
  } -> `Int' #}


{#fun encode_buffer_float as ^
  { `LAME'
  , id `Ptr CFloat' -- ^ PCM data for left channel
  , id `Ptr CFloat' -- ^ PCM data for right channel
  , `Int' -- ^ number of samples per channel
  , id `Ptr CUChar' -- ^ pointer to encoded MP3 stream
  , `Int' -- ^ number of valid octets in this stream
  } -> `Int' #}


{#fun encode_buffer_ieee_float as ^
  { `LAME'
  , id `Ptr CFloat' -- ^ PCM data for left channel
  , id `Ptr CFloat' -- ^ PCM data for right channel
  , `Int' -- ^ number of samples per channel
  , id `Ptr CUChar' -- ^ pointer to encoded MP3 stream
  , `Int' -- ^ number of valid octets in this stream
  } -> `Int' #}


{#fun encode_buffer_interleaved_ieee_float as ^
  { `LAME'
  , id `Ptr CFloat' -- ^ PCM data for left and right channel, interleaved
  , `Int' -- ^ number of samples per channel, /not/ number of samples overall
  , id `Ptr CUChar' -- ^ pointer to encoded MP3 stream
  , `Int' -- ^ number of valid octets in this stream
  } -> `Int' #}


{#fun encode_buffer_ieee_double as ^
  { `LAME'
  , id `Ptr CDouble' -- ^ PCM data for left channel
  , id `Ptr CDouble' -- ^ PCM data for right channel
  , `Int' -- ^ number of samples per channel
  , id `Ptr CUChar' -- ^ pointer to encoded MP3 stream
  , `Int' -- ^ number of valid octets in this stream
  } -> `Int' #}


{#fun encode_buffer_interleaved_ieee_double as ^
  { `LAME'
  , id `Ptr CDouble' -- ^ PCM data for left and right channel, interleaved
  , `Int' -- ^ number of samples per channel, /not/ number of samples overall
  , id `Ptr CUChar' -- ^ pointer to encoded MP3 stream
  , `Int' -- ^ number of valid octets in this stream
  } -> `Int' #}


{#fun encode_buffer_long as ^
  { `LAME'
  , id `Ptr CLong' -- ^ PCM data for left channel
  , id `Ptr CLong' -- ^ PCM data for right channel
  , `Int' -- ^ number of samples per channel
  , id `Ptr CUChar' -- ^ pointer to encoded MP3 stream
  , `Int' -- ^ number of valid octets in this stream
  } -> `Int' #}


{#fun encode_buffer_long2 as ^
  { `LAME'
  , id `Ptr CLong' -- ^ PCM data for left channel
  , id `Ptr CLong' -- ^ PCM data for right channel
  , `Int' -- ^ number of samples per channel
  , id `Ptr CUChar' -- ^ pointer to encoded MP3 stream
  , `Int' -- ^ number of valid octets in this stream
  } -> `Int' #}


{#fun encode_buffer_int as ^
  { `LAME'
  , id `Ptr CInt' -- ^ PCM data for left channel
  , id `Ptr CInt' -- ^ PCM data for right channel
  , `Int' -- ^ number of samples per channel
  , id `Ptr CUChar' -- ^ pointer to encoded MP3 stream
  , `Int' -- ^ number of valid octets in this stream
  } -> `Int' #}


{#fun encode_flush as ^
  { `LAME'
  , id `Ptr CUChar' -- ^ pointer to encoded MP3 stream
  , `Int' -- number of valid octets in this stream
  } -> `Int' #}

{#fun encode_flush_nogap as ^
  { `LAME'
  , id `Ptr CUChar' -- ^ pointer to encoded MP3 stream
  , `Int' -- number of valid octets in this stream
  } -> `Int' #}

{#fun init_bitstream as ^
  { `LAME'
  } -> `Int' #}

-- TODO: some simple statistics

{#fun get_lametag_frame as ^
  { `LAME'
  , id `Ptr CUChar'
  , fromIntegral `CSize'
  } -> `CSize' fromIntegral #}

{#fun close as ^ { `LAME' } -> `Int' #}

{#enum lame_errorcodes_t as ErrorCode {underscoreToCase}
  deriving (Eq, Ord, Show, Read, Bounded) #}

check :: IO Int -> IO ()
check x = x >>= \c -> case c of
  0 -> return ()
  _ -> error $ show (toEnum c :: ErrorCode)
