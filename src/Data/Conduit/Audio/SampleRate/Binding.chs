module Data.Conduit.Audio.SampleRate.Binding
( new, delete, process, reset, setRatio
, State, DataIn(..), DataOut(..), ConverterType(..)
) where

import Foreign hiding (new)
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

{#pointer *SRC_STATE as State newtype #}
{#pointer *SRC_DATA  as Data  newtype #}

{#context prefix="src_"#}

{#fun new as newRaw
  { convTypeToC `ConverterType'
  , `Int'
  , id `Ptr CInt'
  } -> `State' #}

{#fun delete as deleteRaw
  { `State'
  } -> `State' #}

{#fun process as processRaw
  { `State'
  , `Data'
  } -> `Int' #}

{#fun reset as resetRaw
  { `State'
  } -> `Int' #}

{#fun set_ratio as setRatioRaw
  { `State'
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

new :: ConverterType -> Int -> IO State
new ctype chans = alloca $ \perr -> do
  state@(State pstate) <- newRaw ctype chans perr
  when (pstate == nullPtr) $ peek perr >>= sampleRateError "new"
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

data DataIn = DataIn
  { data_in       :: Ptr CFloat
  , data_out      :: Ptr CFloat
  , input_frames  :: Integer
  , output_frames :: Integer
  , src_ratio     :: Double
  , end_of_input  :: Int
  } deriving (Eq, Ord, Show)

data DataOut = DataOut
  { input_frames_used :: Integer
  , output_frames_gen :: Integer
  } deriving (Eq, Ord, Show)

process :: State -> DataIn -> IO DataOut
process state input = allocaBytes {#sizeof SRC_DATA#} $ \pdata -> do
  let sdata = Data pdata
  {#set SRC_DATA.data_in       #} sdata $ data_in                      input
  {#set SRC_DATA.data_out      #} sdata $ data_out                     input
  {#set SRC_DATA.input_frames  #} sdata $ fromIntegral $ input_frames  input
  {#set SRC_DATA.output_frames #} sdata $ fromIntegral $ output_frames input
  {#set SRC_DATA.src_ratio     #} sdata $ realToFrac   $ src_ratio     input
  {#set SRC_DATA.end_of_input  #} sdata $ fromIntegral $ end_of_input  input
  processRaw state sdata >>= sampleRateError "process"
  DataOut
    <$> fmap fromIntegral ({#get SRC_DATA.input_frames_used #} sdata)
    <*> fmap fromIntegral ({#get SRC_DATA.output_frames_gen #} sdata)

delete :: State -> IO ()
delete state = do
  State p <- deleteRaw state
  when (p /= nullPtr) $
    error "Data.Conduit.Audio.SampleRate.delete: returned non-null pointer"

reset :: State -> IO ()
reset state = resetRaw state >>= sampleRateError "reset"

setRatio :: State -> Double -> IO ()
setRatio state r = setRatioRaw state r >>= sampleRateError "setRatio"
