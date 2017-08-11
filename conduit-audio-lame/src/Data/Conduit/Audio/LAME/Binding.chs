module Data.Conduit.Audio.LAME.Binding where

import Foreign
import Foreign.C

#include <lame/lame.h>

{#pointer *lame_global_flags as LAME newtype #}

{#context prefix="lame_"#}

{#fun init as ^ {} -> `LAME' id #}



{#fun get_num_samples as ^ { id `LAME' } -> `Int'  #}
{#fun set_num_samples as ^ { id `LAME',  `Int' } -> `Int' #}
{#fun get_in_samplerate as ^ { id `LAME' } -> `Int'  #}
{#fun set_in_samplerate as ^ { id `LAME',  `Int' } -> `Int' #}
{#fun get_num_channels as ^ { id `LAME' } -> `Int'  #}
{#fun set_num_channels as ^ { id `LAME',  `Int' } -> `Int' #}
{#fun get_scale as ^ { id `LAME' } -> `Float'  #}
{#fun set_scale as ^ { id `LAME',  `Float' } -> `Int' #}
{#fun get_scale_left as ^ { id `LAME' } -> `Float'  #}
{#fun set_scale_left as ^ { id `LAME',  `Float' } -> `Int' #}
{#fun get_scale_right as ^ { id `LAME' } -> `Float'  #}
{#fun set_scale_right as ^ { id `LAME',  `Float' } -> `Int' #}
{#fun get_out_samplerate as ^ { id `LAME' } -> `Int'  #}
{#fun set_out_samplerate as ^ { id `LAME',  `Int' } -> `Int' #}
{#fun get_analysis as ^ { id `LAME' } -> `Bool'  #}
{#fun set_analysis as ^ { id `LAME',  `Bool' } -> `Int' #}
{#fun get_bWriteVbrTag as ^ { id `LAME' } -> `Bool'  #}
{#fun set_bWriteVbrTag as ^ { id `LAME',  `Bool' } -> `Int' #}
{#fun get_decode_only as ^ { id `LAME' } -> `Bool'  #}
{#fun set_decode_only as ^ { id `LAME',  `Bool' } -> `Int' #}
{#fun get_quality as ^ { id `LAME' } -> `Int'  #}
{#fun set_quality as ^ { id `LAME',  `Int' } -> `Int' #}
{#fun get_mode as ^ { id `LAME' } -> `MPEGMode' toCEnum #}
{#fun set_mode as ^ { id `LAME', fromCEnum `MPEGMode' } -> `Int' #}

{#enum MPEG_mode as MPEGMode {underscoreToCase}
  deriving (Eq, Ord, Show, Read, Bounded) #}

fromCEnum :: (Enum a) => a -> CInt
fromCEnum = fromIntegral . fromEnum

toCEnum :: (Enum a) => CInt -> a
toCEnum = toEnum . fromIntegral

{#fun get_force_ms as ^ { id `LAME' } -> `Bool'  #}
{#fun set_force_ms as ^ { id `LAME',  `Bool' } -> `Int' #}
{#fun get_free_format as ^ { id `LAME' } -> `Bool'  #}
{#fun set_free_format as ^ { id `LAME',  `Bool' } -> `Int' #}
{#fun get_findReplayGain as ^ { id `LAME' } -> `Bool'  #}
{#fun set_findReplayGain as ^ { id `LAME',  `Bool' } -> `Int' #}
{#fun get_decode_on_the_fly as ^ { id `LAME' } -> `Bool'  #}
{#fun set_decode_on_the_fly as ^ { id `LAME',  `Bool' } -> `Int' #}
{#fun get_nogap_total as ^ { id `LAME' } -> `Int'  #}
{#fun set_nogap_total as ^ { id `LAME',  `Int' } -> `Int' #}
{#fun get_nogap_currentindex as ^ { id `LAME' } -> `Int'  #}
{#fun set_nogap_currentindex as ^ { id `LAME',  `Int' } -> `Int' #}

-- TODO: lame_set_errorf, lame_set_debugf, lame_set_msgf

{#fun get_brate as ^ { id `LAME' } -> `Int'  #}
{#fun set_brate as ^ { id `LAME',  `Int' } -> `Int' #}
{#fun get_compression_ratio as ^ { id `LAME' } -> `Float'  #}
{#fun set_compression_ratio as ^ { id `LAME',  `Float' } -> `Int' #}

{#fun set_preset as ^ { `LAME', `Int' } -> `Int' #}
{#fun set_asm_optimizations as ^ { `LAME', `Int', `Int' } -> `Int' #}

{#fun get_copyright as ^ { id `LAME' } -> `Bool'  #}
{#fun set_copyright as ^ { id `LAME',  `Bool' } -> `Int' #}
{#fun get_original as ^ { id `LAME' } -> `Bool'  #}
{#fun set_original as ^ { id `LAME',  `Bool' } -> `Int' #}
{#fun get_error_protection as ^ { id `LAME' } -> `Bool'  #}
{#fun set_error_protection as ^ { id `LAME',  `Bool' } -> `Int' #}
{#fun get_extension as ^ { id `LAME' } -> `Bool'  #}
{#fun set_extension as ^ { id `LAME',  `Bool' } -> `Int' #}
{#fun get_strict_ISO as ^ { id `LAME' } -> `Bool'  #}
{#fun set_strict_ISO as ^ { id `LAME',  `Bool' } -> `Int' #}

-- TODO: quantization/noise shaping section

{#fun get_VBR as ^ { id `LAME' } -> `VBRMode' toCEnum #}
{#fun set_VBR as ^ { id `LAME', fromCEnum `VBRMode' } -> `Int' #}

{#enum vbr_mode as VBRMode {underscoreToCase}
  deriving (Eq, Ord, Show, Read, Bounded) #}

{#fun get_VBR_q as ^ { id `LAME' } -> `Int'  #}
{#fun set_VBR_q as ^ { id `LAME',  `Int' } -> `Int' #}
{#fun get_VBR_quality as ^ { id `LAME' } -> `Float'  #}
{#fun set_VBR_quality as ^ { id `LAME',  `Float' } -> `Int' #}

{#fun get_VBR_mean_bitrate_kbps as ^ { id `LAME' } -> `Int'  #}
{#fun set_VBR_mean_bitrate_kbps as ^ { id `LAME',  `Int' } -> `Int' #}
{#fun get_VBR_min_bitrate_kbps as ^ { id `LAME' } -> `Int'  #}
{#fun set_VBR_min_bitrate_kbps as ^ { id `LAME',  `Int' } -> `Int' #}
{#fun get_VBR_max_bitrate_kbps as ^ { id `LAME' } -> `Int'  #}
{#fun set_VBR_max_bitrate_kbps as ^ { id `LAME',  `Int' } -> `Int' #}

{#fun get_VBR_hard_min as ^ { id `LAME' } -> `Bool'  #}
{#fun set_VBR_hard_min as ^ { id `LAME',  `Bool' } -> `Int' #}

{#fun get_lowpassfreq as ^ { id `LAME' } -> `Int'  #}
{#fun set_lowpassfreq as ^ { id `LAME',  `Int' } -> `Int' #}
{#fun get_lowpasswidth as ^ { id `LAME' } -> `Int'  #}
{#fun set_lowpasswidth as ^ { id `LAME',  `Int' } -> `Int' #}
{#fun get_highpassfreq as ^ { id `LAME' } -> `Int'  #}
{#fun set_highpassfreq as ^ { id `LAME',  `Int' } -> `Int' #}
{#fun get_highpasswidth as ^ { id `LAME' } -> `Int'  #}
{#fun set_highpasswidth as ^ { id `LAME',  `Int' } -> `Int' #}

-- TODO: psycho acoustics section

{#fun get_version as ^ { id `LAME' } -> `Int'  #}
{#fun get_encoder_delay as ^ { id `LAME' } -> `Int'  #}
{#fun get_encoder_padding as ^ { id `LAME' } -> `Int'  #}
{#fun get_framesize as ^ { id `LAME' } -> `Int'  #}
{#fun get_mf_samples_to_encode as ^ { id `LAME' } -> `Int'  #}
{#fun get_size_mp3buffer as ^ { id `LAME' } -> `Int'  #}
{#fun get_frameNum as ^ { id `LAME' } -> `Int'  #}
{#fun get_totalframes as ^ { id `LAME' } -> `Int'  #}
{#fun get_RadioGain as ^ { id `LAME' } -> `Int'  #}
{#fun get_AudiophileGain as ^ { id `LAME' } -> `Int'  #}
{#fun get_PeakSample as ^ { id `LAME' } -> `Float'  #}
{#fun get_noclipGainChange as ^ { id `LAME' } -> `Int'  #}
{#fun get_noclipScale as ^ { id `LAME' } -> `Float'  #}

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
  { id `LAME'
  , id `Ptr CShort' -- ^ PCM data for left channel
  , id `Ptr CShort' -- ^ PCM data for right channel
  , `Int' -- ^ number of samples per channel
  , id `Ptr CUChar' -- ^ pointer to encoded MP3 stream
  , `Int' -- ^ number of valid octets in this stream
  } -> `Int' #}


{#fun encode_buffer_interleaved as ^
  { id `LAME'
  , id `Ptr CShort' -- ^ PCM data for left and right channel, interleaved
  , `Int' -- ^ number of samples per channel, /not/ number of samples overall
  , id `Ptr CUChar' -- ^ pointer to encoded MP3 stream
  , `Int' -- ^ number of valid octets in this stream
  } -> `Int' #}


{#fun encode_buffer_float as ^
  { id `LAME'
  , id `Ptr CFloat' -- ^ PCM data for left channel
  , id `Ptr CFloat' -- ^ PCM data for right channel
  , `Int' -- ^ number of samples per channel
  , id `Ptr CUChar' -- ^ pointer to encoded MP3 stream
  , `Int' -- ^ number of valid octets in this stream
  } -> `Int' #}


{#fun encode_buffer_ieee_float as ^
  { id `LAME'
  , id `Ptr CFloat' -- ^ PCM data for left channel
  , id `Ptr CFloat' -- ^ PCM data for right channel
  , `Int' -- ^ number of samples per channel
  , id `Ptr CUChar' -- ^ pointer to encoded MP3 stream
  , `Int' -- ^ number of valid octets in this stream
  } -> `Int' #}


{#fun encode_buffer_interleaved_ieee_float as ^
  { id `LAME'
  , id `Ptr CFloat' -- ^ PCM data for left and right channel, interleaved
  , `Int' -- ^ number of samples per channel, /not/ number of samples overall
  , id `Ptr CUChar' -- ^ pointer to encoded MP3 stream
  , `Int' -- ^ number of valid octets in this stream
  } -> `Int' #}


{#fun encode_buffer_ieee_double as ^
  { id `LAME'
  , id `Ptr CDouble' -- ^ PCM data for left channel
  , id `Ptr CDouble' -- ^ PCM data for right channel
  , `Int' -- ^ number of samples per channel
  , id `Ptr CUChar' -- ^ pointer to encoded MP3 stream
  , `Int' -- ^ number of valid octets in this stream
  } -> `Int' #}


{#fun encode_buffer_interleaved_ieee_double as ^
  { id `LAME'
  , id `Ptr CDouble' -- ^ PCM data for left and right channel, interleaved
  , `Int' -- ^ number of samples per channel, /not/ number of samples overall
  , id `Ptr CUChar' -- ^ pointer to encoded MP3 stream
  , `Int' -- ^ number of valid octets in this stream
  } -> `Int' #}


{#fun encode_buffer_long as ^
  { id `LAME'
  , id `Ptr CLong' -- ^ PCM data for left channel
  , id `Ptr CLong' -- ^ PCM data for right channel
  , `Int' -- ^ number of samples per channel
  , id `Ptr CUChar' -- ^ pointer to encoded MP3 stream
  , `Int' -- ^ number of valid octets in this stream
  } -> `Int' #}


{#fun encode_buffer_long2 as ^
  { id `LAME'
  , id `Ptr CLong' -- ^ PCM data for left channel
  , id `Ptr CLong' -- ^ PCM data for right channel
  , `Int' -- ^ number of samples per channel
  , id `Ptr CUChar' -- ^ pointer to encoded MP3 stream
  , `Int' -- ^ number of valid octets in this stream
  } -> `Int' #}


{#fun encode_buffer_int as ^
  { id `LAME'
  , id `Ptr CInt' -- ^ PCM data for left channel
  , id `Ptr CInt' -- ^ PCM data for right channel
  , `Int' -- ^ number of samples per channel
  , id `Ptr CUChar' -- ^ pointer to encoded MP3 stream
  , `Int' -- ^ number of valid octets in this stream
  } -> `Int' #}


{#fun encode_flush as ^
  { id `LAME'
  , id `Ptr CUChar' -- ^ pointer to encoded MP3 stream
  , `Int' -- number of valid octets in this stream
  } -> `Int' #}

{#fun encode_flush_nogap as ^
  { id `LAME'
  , id `Ptr CUChar' -- ^ pointer to encoded MP3 stream
  , `Int' -- number of valid octets in this stream
  } -> `Int' #}

{#fun init_bitstream as ^
  { id `LAME'
  } -> `Int' #}

-- TODO: some simple statistics

{#fun get_lametag_frame as ^
  { id `LAME'
  , id `Ptr CUChar'
  , fromIntegral `CSize'
  } -> `CSize' fromIntegral #}

{#fun close as ^ { id `LAME' } -> `Int' #}

{#enum lame_errorcodes_t as ErrorCode {underscoreToCase}
  deriving (Eq, Ord, Show, Read, Bounded) #}

check :: IO Int -> IO ()
check x = x >>= \c -> case c of
  0 -> return ()
  _ -> error $ show (toEnum c :: ErrorCode)
