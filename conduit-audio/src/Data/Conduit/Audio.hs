{- |
A high-level functional interface for manipulating streams of audio.
-}
module Data.Conduit.Audio
( -- * Types
  AudioSource(..)
, Seconds, Frames, Rate, Channels, Duration(..)
  -- * Generating audio
, silent, sine
  -- * Combining audio
, concatenate, mix, merge, splitChannels
  -- * Editing audio
, padStart, padEnd
, takeStart, takeEnd
, dropStart, dropEnd
, fadeIn, fadeOut
, mapSamples, gain
  -- * Utility functions
, vectorFrames
, framesToSeconds, secondsToFrames
, chunkSize
, deinterleave, interleave
, integralSample, fractionalSample
, reorganize
) where

import           Control.Monad         (forever, replicateM_, when)
import           Data.Conduit          ((.|))
import qualified Data.Conduit          as C
import           Data.Conduit.Internal (zipSources)
import qualified Data.Conduit.List     as CL
import qualified Data.Vector.Storable  as V
import           Text.Printf           (printf)

-- | An abstraction of a stream of audio. Inside is a Conduit 'C.Source' which
-- loads or generates smallish chunks of audio on demand. @m@ is the 'Monad'
-- used by the 'C.Source' to produce audio. @a@ is the type of audio samples,
-- contained in storable vectors (and thus should be 'V.Storable').
-- Both (signed) 'Integral' and 'Fractional' sample types are supported.
data AudioSource m a = AudioSource
  { source   :: C.ConduitT () (V.Vector a) m ()
  -- ^ The stream of audio chunks; samples interleaved by channel.
  -- Each chunk can be any positive whole number of frames.
  , rate     :: Rate
  , channels :: Channels
  , frames   :: Frames
  -- ^ The stated length in frames of this audio stream.
  -- Not guaranteed to be exactly frame-accurate;
  -- the output of some operations like resampling or time-stretching
  -- may store only approximate frame counts.
  }

-- | A duration in real time.
type Seconds  = Double
-- | A frame consists of one sample for each audio channel.
type Frames   = Int
-- | The number of samples per second.
type Rate     = Double
-- | The number of audio channels (1 = mono, 2 = stereo, etc.)
type Channels = Int

-- | Used for functions that accept durations in either real time or audio frames.
data Duration
  = Seconds Seconds
  | Frames Frames
  deriving (Eq, Ord, Show, Read)

-- | Divides the vector length by the channel count to calculate the number of audio frames.
vectorFrames :: (V.Storable a) => V.Vector a -> Channels -> Frames
vectorFrames v c = case quotRem (V.length v) c of
  (len, 0) -> len
  _        -> error $
    printf "Data.Conduit.Audio.vectorFrames: block length (%d) not divisible by channel count (%d)"
    (V.length v) c

-- | Uses the sample rate to convert frames to seconds.
framesToSeconds :: Frames -> Rate -> Seconds
framesToSeconds fms r = fromIntegral fms / r

-- | Uses the sample rate to convert seconds to frames, rounding if necessary.
secondsToFrames :: Seconds -> Rate -> Frames
secondsToFrames secs r = round $ secs * r

-- | An arbitrary size, in frames, for smallish audio chunks.
chunkSize :: Frames
chunkSize = 5000

-- | Generates a stream of silence with the given parameters.
silent :: (Monad m, Num a, V.Storable a) => Duration -> Rate -> Channels -> AudioSource m a
silent (Seconds secs) r c = silent (Frames $ secondsToFrames secs r) r c
silent (Frames fms) r c = let
  (full, part) = quotRem fms chunkSize
  fullChunk = V.replicate (chunkSize * c) 0
  partChunk = V.replicate (part      * c) 0
  src = do
    replicateM_ full $ C.yield fullChunk
    when (part /= 0) $ C.yield partChunk
  in AudioSource src r c fms

-- | Generates a mono sine wave with the given frequency.
sine :: (Monad m, Floating a, V.Storable a) => a -> Duration -> Rate -> AudioSource m a
sine freq (Seconds secs) r = sine freq (Frames $ secondsToFrames secs r) r
sine freq (Frames fms) r = AudioSource (go 0) r 1 fms where
  valueAt posn = sin $ 2 * pi * freq * (fromIntegral posn / realToFrac r)
  go posn = let
    left = fms - posn
    in if left <= chunkSize
      then C.yield $ V.generate left $ \i -> valueAt $ i + posn
      else let
        firstChunk = V.generate chunkSize $ \i -> valueAt $ i + posn
        in C.yield firstChunk >> go (posn + chunkSize)

-- | Connects the end of the first audio source to the beginning of the second.
-- The two sources must have the same sample rate and channel count.
concatenate :: (Monad m) => AudioSource m a -> AudioSource m a -> AudioSource m a
concatenate (AudioSource s1 r1 c1 l1) (AudioSource s2 r2 c2 l2)
  | r1 /= r2 = error $
    printf "Data.Conduit.Audio.concatenate: mismatched rates (%f and %f)" r1 r2
  | c1 /= c2 = error $
    printf "Data.Conduit.Audio.concatenate: mismatched channel counts (%d and %d)" c1 c2
  | otherwise = AudioSource (s1 >> s2) r1 c1 (l1 + l2)

padStart, padEnd :: (Monad m, Num a, V.Storable a) => Duration -> AudioSource m a -> AudioSource m a
-- | Adds silence to the start of the audio stream.
padStart d src@(AudioSource _ r c _) = concatenate (silent d r c) src
-- | Adds silence to the end of the audio stream.
padEnd   d src@(AudioSource _ r c _) = concatenate src (silent d r c)

-- | Splits an audio stream into several, each providing a single channel of the original stream.
splitChannels :: (Monad m, V.Storable a) => AudioSource m a -> [AudioSource m a]
splitChannels (AudioSource src r c l) = do
  i <- [0 .. c - 1]
  let src' = C.mapOutput (\v -> deinterleave c v !! i) src
  return $ AudioSource src' r 1 l

-- | Mixes two audio streams together by adding them samplewise.
-- The two streams must have the same sample rate and channel count.
-- It is recommended to only mix floating-point sample types.
-- If you mix integral types and the result goes outside of the type's range,
-- the result will not be a normal \"clipping\" effect, but will instead overflow,
-- producing glitchy audio.
mix :: (Monad m, Num a, V.Storable a) => AudioSource m a -> AudioSource m a -> AudioSource m a
mix a1@(AudioSource _ r1 c1 l1) a2@(AudioSource _ r2 c2 l2)
  | r1 /= r2 = error $
    printf "Data.Conduit.Audio.mix: mismatched rates (%f and %f)" r1 r2
  | c1 /= c2 = error $
    printf "Data.Conduit.Audio.mix: mismatched channel counts (%d and %d)" c1 c2
  | otherwise = AudioSource
    (C.mapOutput (uncurry $ V.zipWith (+)) $ combineAudio a1 a2)
    r1 c1 (max l1 l2)

-- | Combines the channels of two audio streams into a single source with all the channels.
-- The two streams must have the same sample rate, but can have any number of channels.
merge :: (Monad m, Num a, V.Storable a) => AudioSource m a -> AudioSource m a -> AudioSource m a
merge a1@(AudioSource _ r1 c1 l1) a2@(AudioSource _ r2 c2 l2)
  | r1 /= r2 = error $
    printf "Data.Conduit.Audio.merge: mismatched rates (%f and %f)" r1 r2
  | otherwise = AudioSource
    (C.mapOutput mergeChunk $ combineAudio a1 a2)
    r1 (c1 + c2) (max l1 l2)
  where mergeChunk (p1, p2) = interleave $ deinterleave c1 p1 ++ deinterleave c2 p2

-- | Applies a function to every sample in the audio stream.
mapSamples :: (Monad m, V.Storable a, V.Storable b) =>
  (a -> b) -> AudioSource m a -> AudioSource m b
mapSamples f (AudioSource s r c l) = AudioSource (C.mapOutput (V.map f) s) r c l

-- | Multiplies all the audio samples by the given scaling factor.
-- It is best to use this function on floating-point sample types,
-- for the same reasons that apply to 'mix'.
gain :: (Monad m, Num a, V.Storable a) => a -> AudioSource m a -> AudioSource m a
gain d = mapSamples (* d)

-- | Fades the audio from start (silent) to end (original volume).
-- This function relies on the 'frames' value stored with the stream.
fadeIn :: (Monad m, Ord a, Fractional a, V.Storable a) => AudioSource m a -> AudioSource m a
fadeIn (AudioSource s r c l) = let
  go i = C.await >>= \mx -> case mx of
    Nothing -> return ()
    Just v  -> let
      fader = V.generate (V.length v) $ \j ->
        min 1 $ fromIntegral (i + quot j c) / fromIntegral l
      in C.yield (V.zipWith (*) v fader) >> go (i + vectorFrames v c)
  in AudioSource (s .| go 0) r c l

-- | Fades the audio from start (original volume) to end (silent).
-- This function relies on the 'frames' value stored with the stream.
fadeOut :: (Monad m, Ord a, Fractional a, V.Storable a) => AudioSource m a -> AudioSource m a
fadeOut (AudioSource s r c l) = let
  go i = C.await >>= \mx -> case mx of
    Nothing -> return ()
    Just v  -> let
      fader = V.generate (V.length v) $ \j ->
        1 - (min 1 $ fromIntegral (i + quot j c) / fromIntegral l)
      in C.yield (V.zipWith (*) v fader) >> go (i + vectorFrames v c)
  in AudioSource (s .| go 0) r c l

-- | Takes no more than the given duration of audio from the start of the stream.
takeStart :: (Monad m, V.Storable a) => Duration -> AudioSource m a -> AudioSource m a
takeStart (Seconds secs) src = takeStart (Frames $ secondsToFrames secs $ rate src) src
takeStart (Frames fms) (AudioSource src r c l) = let
  go left = C.await >>= \mx -> case mx of
    Nothing -> return ()
    Just v  -> let
      len = V.length v
      in case compare left len of
        EQ -> C.yield v
        LT -> C.yield $ V.take left v
        GT -> C.yield v >> go (left - len)
  in AudioSource (src .| go (fms * c)) r c (min l fms)

-- | Drops the given duration of audio from the start of the stream.
dropStart :: (Monad m, V.Storable a) => Duration -> AudioSource m a -> AudioSource m a
dropStart (Seconds secs) src = dropStart (Frames $ secondsToFrames secs $ rate src) src
dropStart (Frames fms) (AudioSource src r c l) = let
  go left = C.await >>= \mx -> case mx of
    Nothing -> return ()
    Just v  -> let
      len = V.length v
      in case compare left len of
        EQ -> CL.map id
        LT -> C.yield (V.drop left v) >> CL.map id
        GT -> go (left - len)
  in AudioSource (src .| go (fms * c)) r c (max 0 $ l - fms)

takeEnd, dropEnd :: (Monad m, V.Storable a) => Duration -> AudioSource m a -> AudioSource m a
-- | Takes no more than the given duration of audio from the end of the stream.
-- This function relies on the 'frames' value stored with the stream.
takeEnd (Frames fms) src = dropStart (Frames $ frames src - fms) src
takeEnd (Seconds secs) src = takeEnd (Frames $ secondsToFrames secs $ rate src) src
-- | Drops the given duration of audio from the end of the stream.
-- This function relies on the 'frames' value stored with the stream.
dropEnd (Frames fms) src = takeStart (Frames $ frames src - fms) src
dropEnd (Seconds secs) src = dropEnd (Frames $ secondsToFrames secs $ rate src) src

-- | Given a vector with interleaved samples, like @[L0, R0, L1, R1, ...]@,
-- converts it into @[[L0, L1, ...], [R0, R1, ...]]@.
deinterleave :: (V.Storable a) => Channels -> V.Vector a -> [V.Vector a]
deinterleave n v = do
  let fms = vectorFrames v n
  i <- [0 .. n - 1]
  return $ V.generate fms $ \j -> v V.! (n * j + i)

-- | Opposite of 'deinterleave'.
-- All the input vectors should have the same length.
interleave :: (V.Storable a) => [V.Vector a] -> V.Vector a
interleave vs = let
  n = length vs
  in V.generate (sum $ map V.length vs) $ \i -> let
    (q, r) = quotRem i n
    in (vs !! r) V.! q

-- | Combines two audio streams to produce pairs of same-length (in frames) chunks.
-- If one stream is shorter, its end will be padded with silence to match the longer one.
-- This function is used to implement 'mix' and 'merge'.
combineAudio :: (Monad m, V.Storable a, V.Storable b, Num a, Num b)
  => AudioSource m a -> AudioSource m b -> C.ConduitT () (V.Vector a, V.Vector b) m ()
combineAudio src1 src2 = let
  org1 = justify $ source $ reorganize chunkSize src1
  org2 = justify $ source $ reorganize chunkSize src2
  justify src = C.mapOutput Just src >> forever (C.yield Nothing)
  in zipSources org1 org2 .| let
    loop = C.await >>= \mp -> case mp of
      Nothing -> error "Data.Conduit.Audio.combineAudio: internal error! reached end of infinite stream"
      Just p -> case p of
        (Nothing, Nothing) -> return ()
        (Nothing, Just v2) -> let
          v1 = V.replicate (vectorFrames v2 (channels src2) * channels src1) 0
          in C.yield (v1, v2) >> loop
        (Just v1, Nothing) -> let
          v2 = V.replicate (vectorFrames v1 (channels src1) * channels src2) 0
          in C.yield (v1, v2) >> loop
        (Just v1, Just v2) -> let
          len1 = vectorFrames v1 (channels src1)
          len2 = vectorFrames v2 (channels src2)
          in case compare len1 len2 of
            EQ -> C.yield (v1, v2) >> loop
            LT -> let
              v1' = v1 V.++ V.replicate ((len2 - len1) * channels src1) 0
              in C.yield (v1', v2) >> loop
            GT -> let
              v2' = v2 V.++ V.replicate ((len1 - len2) * channels src2) 0
              in C.yield (v1, v2') >> loop
    in loop

-- See http://blog.bjornroche.com/2009/12/int-float-int-its-jungle-out-there.html
-- for a discussion of different int/float sample conversion methods.
-- The ones below multiply/divide by 0x7FFF (or equivalent).
-- Int16 -> Float -> Int16 conversions are transparent:
-- all (\i -> i == integralSample (fractionalSample i :: Float)) [minBound :: Int16 .. maxBound]

-- | Converts fractional samples in the range @[-1, 1]@ to integral samples
-- in a two's-complement type. Fractional samples beyond that range are clamped.
integralSample :: (RealFrac a, Integral b, Bounded b) => a -> b
integralSample x
  | x <= (-1) = minBound
  | x >= 1    = maxBound
  | otherwise = let
    result = round $ x * fromIntegral (maxBound `asTypeOf` result)
    in result

-- | Converts integral samples in a two's-complement type to fractional
-- samples in the range @[-1, 1]@.
fractionalSample :: (Integral a, Bounded a, Fractional b) => a -> b
fractionalSample x = fromIntegral x / fromIntegral (maxBound `asTypeOf` x)

reorganizer :: (Monad m, V.Storable a) => Int -> C.ConduitT (V.Vector a) (V.Vector a) m ()
reorganizer samps = let
  go = C.await >>= \ml -> case ml of
    Nothing -> return ()
    Just v  -> case compare samps $ V.length v of
      EQ -> C.yield v >> go
      LT -> case V.splitAt samps v of
        (v1, v2) -> C.yield v1 >> C.leftover v2 >> go
      GT -> C.await >>= \m2 -> case m2 of
        Nothing -> C.yield v
        Just v' -> C.leftover (v V.++ v') >> go
  in go

-- | Modifies the source so that it outputs vectors of a consistent length in frames.
-- The last vector from the new source may be less than the given length.
reorganize :: (Monad m, V.Storable a) => Frames -> AudioSource m a -> AudioSource m a
reorganize fms src = src { source = source src .| reorganizer (fms * channels src) }
