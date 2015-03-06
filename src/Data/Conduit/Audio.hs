{-# LANGUAGE LambdaCase #-}
module Data.Conduit.Audio where

import qualified Data.Vector.Storable as V
import qualified Data.Conduit as C
import Data.Conduit ((=$=))
import qualified Data.Conduit.List as CL
import Data.Conduit.Internal (zipSources)
import Control.Monad (replicateM_, forever)
import Data.Maybe (fromMaybe)

newtype Audio = Audio { audioData :: [V.Vector Float] }

type Seconds  = Double
type Samples  = Integer
type Channels = Integer

audioLength :: Audio -> Samples
audioLength (Audio vs) = fromIntegral $ V.length $ head vs

silence :: Samples -> Channels -> Audio
silence samps chans =
  Audio $ replicate (fromIntegral chans) $ V.replicate (fromIntegral samps) 0

silent :: (Monad m) => Samples -> Channels -> C.Source m Audio
silent samps chans = let
  chunkSize = 10000
  (q, r) = quotRem samps chunkSize
  fullChunk = silence chunkSize chans
  partChunk = silence r chans
  in do
    replicateM_ (fromIntegral q) $ C.yield fullChunk
    C.yield partChunk

merge :: (Monad m) => C.Source m Audio -> C.Source m Audio -> C.Source m Audio
merge x y = let
  pairs = combineAudio (x =$= CL.map audioData) (y =$= CL.map audioData)
  mergePair (vs1, vs2) = Audio $ vs1 ++ vs2
  in pairs =$= CL.map mergePair

mix :: (Monad m) => C.Source m Audio -> C.Source m Audio -> C.Source m Audio
mix x y = let
  pairs = combineAudio (x =$= CL.map audioData) (y =$= CL.map audioData)
  mergePair (vs1, vs2) = Audio $ zipWith (V.zipWith (+)) vs1 vs2
  in pairs =$= CL.map mergePair

combineAudio
  :: (Num a, V.Storable a, Monad m)
  => C.Source m [V.Vector a]
  -> C.Source m [V.Vector a]
  -> C.Source m ([V.Vector a], [V.Vector a])
combineAudio s1 s2 = let
  justify src = (src =$= CL.map Just) >> forever (C.yield Nothing)
  nothingPanic = error "combineAudio: internal error! reached end of infinite stream"
  zeroOut = map $ V.map $ const 0
  in zipSources (justify s1) (justify s2) =$= let
    loop = C.await >>= \case
      Nothing -> nothingPanic
      Just pair -> case pair of
        (Nothing , Nothing ) -> return ()
        (Just vs1, Nothing ) -> C.yield (vs1, zeroOut vs1) >> loop
        (Nothing , Just vs2) -> C.yield (zeroOut vs2, vs2) >> loop
        (Just vs1, Just vs2) -> case compare (V.length $ head vs1) (V.length $ head vs2) of
          EQ -> C.yield (vs1, vs2) >> loop
          LT -> let
            vs2split = map (V.splitAt $ V.length $ head vs1) vs2
            (vs2a, vs2b) = (map fst vs2split, map snd vs2split)
            in C.yield (vs1, vs2a) >> C.await >>= \case
              Nothing -> nothingPanic
              Just (next1, next2) -> do
                C.leftover (next1, Just $ zipWith (V.++) vs2b $ fromMaybe (repeat V.empty) next2)
                loop
          GT -> let -- TODO: check this
            vs1split = map (V.splitAt $ V.length $ head vs2) vs1
            (vs1a, vs1b) = (map fst vs1split, map snd vs1split)
            in C.yield (vs1a, vs2) >> C.await >>= \case
              Nothing -> nothingPanic
              Just (next1, next2) -> do
                C.leftover (Just $ zipWith (V.++) vs1b $ fromMaybe (repeat V.empty) next1, next2)
                loop
    in loop

gain :: (Monad m) => Float -> C.Conduit Audio m Audio
gain d = CL.map $ \(Audio vs) -> Audio $ map (V.map (* d)) vs

dropStart :: (Monad m) => Samples -> C.Conduit Audio m Audio
dropStart s = C.await >>= \case
  Nothing         -> return ()
  Just (Audio vs) -> let
    len = fromIntegral $ V.length $ head vs
    in case compare s len of
      EQ -> CL.map id
      LT -> C.yield (Audio $ map (V.drop $ fromIntegral s) vs) >> CL.map id
      GT -> dropStart $ s - len

fadeIn :: (Monad m) => Samples -> C.Conduit Audio m Audio
fadeIn samps = let
  go i = if i >= samps
    then CL.map id
    else C.await >>= \case
      Nothing -> return ()
      Just chunk@(Audio vs) -> let
        fader :: V.Vector Float
        fader = V.generate (fromIntegral $ audioLength chunk) $ \j ->
          min 1 $ fromIntegral j / fromIntegral samps
        faded = Audio $ map (V.zipWith (*) fader) vs
        in C.yield faded >> go (i + audioLength chunk)
  in go 0

fadeOut :: (Monad m) => Samples -> C.Conduit Audio m Audio
fadeOut = undefined

takeStart :: (Monad m) => Samples -> C.Conduit Audio m Audio
takeStart s = C.await >>= \case
  Nothing         -> return ()
  Just (Audio vs) -> let
    len = fromIntegral $ V.length $ head vs
    in case compare s len of
      EQ -> C.yield $ Audio vs
      LT -> C.yield $ Audio $ map (V.take $ fromIntegral s) vs
      GT -> C.yield (Audio vs) >> takeStart (s - len)
