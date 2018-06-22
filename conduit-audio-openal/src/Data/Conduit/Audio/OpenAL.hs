{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NondecreasingIndentation #-}
module Data.Conduit.Audio.OpenAL where

import           Control.Concurrent           (threadDelay)
import           Control.Concurrent.MVar
import           Control.Exception            (bracket)
import           Control.Monad.Fix            (fix)
import           Foreign
import           Foreign.C
import           Sound.AL
import           Sound.ALC
import           System.IO.Unsafe             (unsafePerformIO)

import           Control.Concurrent           (forkIO)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Trans.Resource
import           Data.Conduit.Audio
import qualified Data.Vector.Storable         as V

makeBuffer :: (Storable a) => ALenum -> ALsizei -> V.Vector a -> IO ALuint
makeBuffer fmt freq v = do
  buf <- alloca $ \p -> do
    check $ alGenBuffers 1 p
    peek p
  V.unsafeWith v $ \p -> do
    let len = fromIntegral $ V.length v * sizeOf (V.head v)
    check $ alBufferData buf fmt p len freq
  return buf

data SmartSource = SmartSource
  { rawSource    :: ALuint
  , fullSource   :: MVar ()
  , deleteSource :: MVar ()
  }

sourceToAL :: (MonadResource m) => AudioSource m Int16 -> m SmartSource
sourceToAL audio = do
  bufFormat <- case channels audio of
    1 -> return al_FORMAT_MONO16
    2 -> return al_FORMAT_STEREO16
    c -> error $
      "sourceToAL: only mono/stereo supported, " ++ show c ++ " channels given"
  src <- liftIO $ alloca $ \p -> do
    check $ alGenSources 1 p
    peek p
  check $ alSourcef src al_PITCH 1
  check $ alSourcef src al_GAIN 1
  check $ alSource3f src al_POSITION 0 0 0
  check $ alSource3f src al_VELOCITY 0 0 0
  check $ alSourcei src al_LOOPING al_FALSE
  del <- liftIO newEmptyMVar
  full <- liftIO newEmptyMVar
  _ <- liftIO $ forkIO undefined
  return SmartSource
    { rawSource = src
    , deleteSource = del
    , fullSource = full
    }

main :: IO ()
main = do
  getDevices >>= print
  bracket (check $ alcOpenDevice nullPtr) (check . alcCloseDevice) $ \dev -> do
  bracket (check $ alcCreateContext dev nullPtr) (check . alcDestroyContext) $ \ctx -> do
  True <- alToBool <$> check (alcMakeContextCurrent ctx)
  src <- alloca $ \p -> do
    check $ alGenSources 1 p
    peek p
  check $ alSourcef src al_PITCH 1
  check $ alSourcef src al_GAIN 1
  check $ alSource3f src al_POSITION 0 0 0
  check $ alSource3f src al_VELOCITY 0 0 0
  check $ alSourcei src al_LOOPING al_FALSE
  buf <- alloca $ \p -> do
    check $ alGenBuffers 1 p
    peek p
  withArray chromatic $ \bufData -> do
    let len = fromIntegral $ length chromatic * 2
    check $ alBufferData buf al_FORMAT_MONO16 bufData len 44100
  check $ alSourcei src al_BUFFER $ fromIntegral buf
  check $ alSourcePlay src
  fix $ \loop -> do
    srcState <- alloca $ \p -> do
      check $ alGetSourcei src al_SOURCE_STATE p
      peek p
    if srcState == al_PLAYING
      then threadDelay 5000 >> loop
      else return ()

chromatic :: [Int16]
chromatic = do
  steps <- [0 .. 12]
  let step = 2 ** (1 / 12)
      freq = 220 * (step ** steps)
  take 22050 $ monoFreq freq

monoFreq :: Float -> [Int16]
monoFreq freq = let
  srate = 44100
  phase = 0
  y t = (sin $ 2 * pi * freq * t + phase) ** 0.25
  floatToInt16 :: Float -> Int16
  floatToInt16 f = round $ f * 32767
  in map (\s -> floatToInt16 $ y $ fromIntegral s / srate) ([0 ..] :: [Int])

-- | Reads a string and also returns the position after the string's terminator.
consumeCString :: Ptr ALubyte -> IO (String, Ptr ALubyte)
consumeCString p = do
  len <- lengthArray0 0 p
  str <- peekCStringLen (castPtr p, len)
  return (str, p `plusPtr` (len + 1))

getDevices :: IO [String]
getDevices = do
  let loop p = consumeCString p >>= \case
        ("" , _ ) -> return []
        (str, p') -> (str :) <$> loop p'
  check (alcGetString nullPtr alc_DEVICE_SPECIFIER) >>= loop

{-# NOINLINE lockAL #-}
lockAL :: MVar ()
lockAL = unsafePerformIO $ newMVar ()

check :: (MonadIO m) => IO a -> m a
check act = liftIO $ withMVar lockAL $ \() -> do
  _ <- alGetError
  x <- act
  err <- alGetError
  if err == fromIntegral al_NO_ERROR
    then return x
    else error $ "OpenAL error: " ++ show err

alToBool :: (Integral a, Show a) => a -> Bool
alToBool b
  | fromIntegral b == al_TRUE  = True
  | fromIntegral b == al_FALSE = False
  | otherwise                  = error $ "alToBool: unknown value " ++ show b
