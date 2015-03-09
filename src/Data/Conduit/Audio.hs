{-# LANGUAGE LambdaCase #-}
module Data.Conduit.Audio where

import qualified Data.Vector.Storable as V
import qualified Data.Conduit as C
import Data.Conduit ((=$=))
import qualified Data.Conduit.List as CL
import Data.Conduit.Internal (zipSources)
import Control.Monad (replicateM_, forever, when)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)

data AudioSource m = AudioSource
  { source   :: C.Source m (V.Vector Float)
  -- ^ The stream of audio chunks; floating-point samples,
  -- interleaved by channel. Each chunk can be any positive whole number
  -- of frames.
  , rate     :: Rate
  , channels :: Channels
  , frames   :: Frames
  -- ^ The stated length in frames of this audio stream.
  -- Not guaranteed to be exactly frame-accurate, but should be close.
  }

type Seconds  = Double
type Frames   = Int
type Rate     = Double
type Channels = Int

vectorFrames :: V.Vector Float -> Channels -> Frames
vectorFrames v c = case quotRem (V.length v) c of
  (len, 0) -> len
  _        -> error $
    printf "Data.Conduit.Audio.vectorFrames: block length (%d) not divisible by channel count (%d)"
    (V.length v) c

framesToSeconds :: Frames -> Rate -> Seconds
framesToSeconds fms r = fromIntegral fms / r

secondsToFrames :: Seconds -> Rate -> Frames
secondsToFrames secs r = round $ secs * r

-- | An arbitrary size, in frames, for audio chunks.
chunkSize :: Frames
chunkSize = 10000

silentFrames :: (Monad m) => Frames -> Rate -> Channels -> AudioSource m
silentFrames fms r c = let
  (full, part) = quotRem chunkSize fms
  fullChunk = V.replicate (chunkSize * c) 0
  partChunk = V.replicate (part      * c) 0
  src = do
    replicateM_ full $ C.yield fullChunk
    when (part /= 0) $ C.yield partChunk
  in AudioSource src r c fms

silent :: (Monad m) => Seconds -> Rate -> Channels -> AudioSource m
silent secs r = silentFrames (secondsToFrames secs r) r

concatenate :: (Monad m) => AudioSource m -> AudioSource m -> AudioSource m
concatenate (AudioSource s1 r1 c1 l1) (AudioSource s2 r2 c2 l2)
  | r1 /= r2 = error $
    printf "Data.Conduit.Audio.concatenate: mismatched rates (%d and %d)" r1 r2
  | c1 /= c2 = error $
    printf "Data.Conduit.Audio.concatenate: mismatched channel counts (%d and %d)" c1 c2
  | otherwise = AudioSource (s1 >> s2) r1 c1 (l1 + l2)

padStartFrames, padEndFrames :: (Monad m) => Frames -> AudioSource m -> AudioSource m
padStartFrames fms src@(AudioSource _ r c _) = concatenate (silentFrames fms r c) src
padEndFrames   fms src@(AudioSource _ r c _) = concatenate src (silentFrames fms r c)

padStart, padEnd :: (Monad m) => Seconds -> AudioSource m -> AudioSource m
padStart secs src@(AudioSource _ r c _) = concatenate (silent secs r c) src
padEnd   secs src@(AudioSource _ r c _) = concatenate src (silent secs r c)

splitChannels :: (Monad m) => AudioSource m -> [AudioSource m]
splitChannels (AudioSource src r c l) = do
  i <- [0 .. c - 1]
  let src' = src =$= CL.map (\v -> deinterleave c v !! i)
  return $ AudioSource src' r 1 l

mix :: (Monad m) => AudioSource m -> AudioSource m -> AudioSource m
mix (AudioSource s1 r1 c1 l1) (AudioSource s2 r2 c2 l2)
  | r1 /= r2 = error $
    printf "Data.Conduit.Audio.mix: mismatched rates (%d and %d)" r1 r2
  | c1 /= c2 = error $
    printf "Data.Conduit.Audio.mix: mismatched channel counts (%d and %d)" c1 c2
  | otherwise = AudioSource
    (combineAudio s1 s2 =$= CL.map (uncurry $ V.zipWith (+)))
    r1 c1 (max l1 l2)

merge :: (Monad m) => AudioSource m -> AudioSource m -> AudioSource m
merge (AudioSource s1 r1 c1 l1) (AudioSource s2 r2 c2 l2)
  | r1 /= r2 = error $
    printf "Data.Conduit.Audio.merge: mismatched rates (%d and %d)" r1 r2
  | otherwise = AudioSource
    (combineAudio s1 s2 =$= CL.map
      (\(p1, p2) -> interleave $ deinterleave c1 p1 ++ deinterleave c2 p2))
    r1 (c1 + c2) (max l1 l2)

gain :: (Monad m) => Float -> AudioSource m -> AudioSource m
gain d (AudioSource s r c l) = AudioSource (s =$= CL.map (V.map (* d))) r c l

fadeIn :: (Monad m) => AudioSource m -> AudioSource m
fadeIn (AudioSource s r c l) = let
  go i = C.await >>= \case
    Nothing -> return ()
    Just v  -> let
      fader = V.generate (V.length v) $ \j ->
        min 1 $ fromIntegral (i + quot j c) / fromIntegral l
      in C.yield (V.zipWith (*) v fader) >> go (i + vectorFrames v c)
  in AudioSource (s =$= go 0) r c l

fadeOut :: (Monad m) => AudioSource m -> AudioSource m
fadeOut (AudioSource s r c l) = let
  go i = C.await >>= \case
    Nothing -> return ()
    Just v  -> let
      fader = V.generate (V.length v) $ \j ->
        1 - (min 1 $ fromIntegral (i + quot j c) / fromIntegral l)
      in C.yield (V.zipWith (*) v fader) >> go (i + vectorFrames v c)
  in AudioSource (s =$= go 0) r c l

takeStartFrames :: (Monad m) => Frames -> AudioSource m -> AudioSource m
takeStartFrames fms (AudioSource src r c l) = let
  go left = C.await >>= \case
    Nothing -> return ()
    Just v  -> let
      len = V.length v
      in case compare left len of
        EQ -> C.yield v
        LT -> C.yield $ V.take left v
        GT -> C.yield v >> go (left - len)
  in AudioSource (src =$= go (fms * c)) r c (min l fms)

dropStartFrames :: (Monad m) => Frames -> AudioSource m -> AudioSource m
dropStartFrames fms (AudioSource src r c l) = let
  go left = C.await >>= \case
    Nothing -> return ()
    Just v  -> let
      len = V.length v
      in case compare left len of
        EQ -> CL.map id
        LT -> C.yield (V.drop left v) >> CL.map id
        GT -> go (left - len)
  in AudioSource (src =$= go (fms * c)) r c (max 0 $ l - fms)

takeEndFrames, dropEndFrames :: (Monad m) => Frames -> AudioSource m -> AudioSource m
takeEndFrames fms src = dropStartFrames (frames src - fms) src
dropEndFrames fms src = takeStartFrames (frames src - fms) src

takeStart, takeEnd, dropStart, dropEnd :: (Monad m) => Seconds -> AudioSource m -> AudioSource m
takeStart secs src = takeStartFrames (secondsToFrames secs $ rate src) src
takeEnd   secs src = takeEndFrames   (secondsToFrames secs $ rate src) src
dropStart secs src = dropStartFrames (secondsToFrames secs $ rate src) src
dropEnd   secs src = dropEndFrames   (secondsToFrames secs $ rate src) src

-- | Given a vector with interleaved samples, like @[L0, R0, L1, R1, ...]@,
-- converts it into @[[L0, L1, ...], [R0, R1, ...]]@.
deinterleave :: (V.Storable a) => Channels -> V.Vector a -> [V.Vector a]
deinterleave n v = do
  let len = V.length v `div` n
  i <- [0 .. n - 1]
  return $ V.generate len $ \j -> v V.! (n * j + i)

interleave :: (V.Storable a) => [V.Vector a] -> V.Vector a
interleave vs = let
  n = length vs
  in V.generate (sum $ map V.length vs) $ \i -> let
    (q, r) = quotRem i n
    in (vs !! r) V.! q

combineAudio
  :: (Num a, V.Storable a, Monad m)
  => C.Source m (V.Vector a)
  -> C.Source m (V.Vector a)
  -> C.Source m (V.Vector a, V.Vector a)
combineAudio s1 s2 = let
  justify src = (src =$= CL.map Just) >> forever (C.yield Nothing)
  nothingPanic = error "combineAudio: internal error! reached end of infinite stream"
  zeroOut = V.map $ const 0
  in zipSources (justify s1) (justify s2) =$= let
    loop = C.await >>= \case
      Nothing -> nothingPanic
      Just pair -> case pair of
        (Nothing, Nothing) -> return ()
        (Just v1, Nothing) -> C.yield (v1, zeroOut v1) >> loop
        (Nothing, Just v2) -> C.yield (zeroOut v2, v2) >> loop
        (Just v1, Just v2) -> case compare (V.length v1) (V.length v2) of
          EQ -> C.yield (v1, v2) >> loop
          LT -> let
            (v2a, v2b) = V.splitAt (V.length v1) v2
            in C.yield (v1, v2a) >> C.await >>= \case
              Nothing -> nothingPanic
              Just (next1, next2) -> do
                C.leftover (next1, Just $ v2b V.++ fromMaybe V.empty next2)
                loop
          GT -> let
            (v1a, v1b) = V.splitAt (V.length v2) v1
            in C.yield (v1a, v2) >> C.await >>= \case
              Nothing -> nothingPanic
              Just (next1, next2) -> do
                C.leftover (Just $ v1b V.++ fromMaybe V.empty next1, next2)
                loop
    in loop
