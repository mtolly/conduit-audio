module Data.Conduit.Audio.SampleRate.Binding
( newState, delete, process, reset, setRatio
, SRC_STATE, SRC_DATA_input(..), SRC_DATA_output(..), ConverterType(..)
) where

import Foreign
import Foreign.C
import Control.Monad (when)
import Control.Applicative

#include <samplerate.h>

{-
SRC_STATE* src_new (int converter_type, int channels, int *error) ;
SRC_STATE* src_delete (SRC_STATE *state) ;

int src_process (SRC_STATE *state, SRC_DATA *data) ;
int src_reset (SRC_STATE *state) ;
int src_set_ratio (SRC_STATE *state, double new_ratio) ;
-}

{#pointer *SRC_STATE newtype #}
{#pointer *SRC_DATA  newtype #}

{#context prefix="src_"#}

{#fun new as newRaw
  { convTypeToC `ConverterType'
  , `Int'
  , id `Ptr CInt'
  } -> `SRC_STATE' #}

{#fun delete as deleteRaw
  { `SRC_STATE'
  } -> `SRC_STATE' #}

{#fun process as processRaw
  { `SRC_STATE'
  , `SRC_DATA'
  } -> `Int' #}

{#fun reset as resetRaw
  { `SRC_STATE'
  } -> `Int' #}

{#fun set_ratio as setRatioRaw
  { `SRC_STATE'
  , `Double'
  } -> `Int' #}

{#enum define ConverterType
  { SRC_SINC_BEST_QUALITY as SincBestQuality
  , SRC_SINC_MEDIUM_QUALITY as SincMediumQuality
  , SRC_SINC_FASTEST as SincFastest
  , SRC_ZERO_ORDER_HOLD as ZeroOrderHold
  , SRC_LINEAR as Linear
  } deriving (Eq, Ord, Show, Read, Bounded) #}

convTypeToC :: ConverterType -> CInt
convTypeToC = fromIntegral . fromEnum

-- const char* src_strerror (int error) ;

{#fun strerror as ^
  { id `CInt'
  } -> `CString' id #}

sampleRateError :: (Integral i) => String -> i -> IO ()
sampleRateError _  0 = return ()
sampleRateError fn i = do
  ps <- strerror $ fromIntegral i
  s <- if ps == nullPtr
    then return "strerror returned NULL"
    else peekCString ps
  error $ "Data.Conduit.Audio.SampleRate." ++ fn ++ ": libsamplerate error; " ++ s

newState :: ConverterType -> Int -> IO SRC_STATE
newState ctype chans = alloca $ \perr -> do
  state@(SRC_STATE pstate) <- newRaw ctype chans perr
  when (pstate == nullPtr) $ peek perr >>= sampleRateError "newState"
  return state

{-
typedef struct
{   float  *data_in, *data_out ;

    long   input_frames, output_frames ;
    long   input_frames_used, output_frames_gen ;

    int    end_of_input ;

    double src_ratio ;
} SRC_DATA ;
-}

data SRC_DATA_input = SRC_DATA_input
  { data_in :: Ptr CFloat
  , data_out :: Ptr CFloat
  , input_frames :: CLong
  , output_frames :: CLong
  , src_ratio :: CDouble
  , end_of_input :: CInt
  } deriving (Eq, Ord, Show)

data SRC_DATA_output = SRC_DATA_output
  { input_frames_used :: CLong
  , output_frames_gen :: CLong
  } deriving (Eq, Ord, Show)

process :: SRC_STATE -> SRC_DATA_input -> IO SRC_DATA_output
process state input = allocaBytes {#sizeof SRC_DATA#} $ \pdata -> do
  let sdata = SRC_DATA pdata
  {#set SRC_DATA.data_in       #} sdata $ data_in       input
  {#set SRC_DATA.data_out      #} sdata $ data_out      input
  {#set SRC_DATA.input_frames  #} sdata $ input_frames  input
  {#set SRC_DATA.output_frames #} sdata $ output_frames input
  {#set SRC_DATA.src_ratio     #} sdata $ src_ratio     input
  {#set SRC_DATA.end_of_input  #} sdata $ end_of_input  input
  processRaw state sdata >>= sampleRateError "process"
  SRC_DATA_output
    <$> {#get SRC_DATA.input_frames_used #} sdata
    <*> {#get SRC_DATA.output_frames_gen #} sdata

delete :: SRC_STATE -> IO ()
delete state = do
  SRC_STATE p <- deleteRaw state
  when (p /= nullPtr) $
    error "Data.Conduit.Audio.SampleRate.delete: returned non-null pointer"

reset :: SRC_STATE -> IO ()
reset state = resetRaw state >>= sampleRateError "reset"

setRatio :: SRC_STATE -> Double -> IO ()
setRatio state r = setRatioRaw state r >>= sampleRateError "setRatio"
