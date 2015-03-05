{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
module Data.Conduit.Audio where

import GHC.TypeLits
import qualified Data.Vector.Storable as V
import qualified Data.Conduit as C
import Data.Conduit ((=$=))
import qualified Data.Conduit.List as CL
import Data.Conduit.Internal (zipSources)
import Data.Proxy
import Control.Monad (replicateM_, forever)
import Data.Maybe (fromMaybe)

newtype Audio (r :: Nat) (c :: Nat) = Audio { audioData :: [V.Vector Float] }

type Seconds  = Double
type Samples  = Integer
type Channels = Integer

rate :: (KnownNat r) => Audio r c -> Samples
rate = let
  rateProxy :: Audio r c -> Proxy r
  rateProxy _ = Proxy
  in natVal . rateProxy

channels :: (KnownNat c) => Audio r c -> Channels
channels = natVal

silenceSamples :: (KnownNat r, KnownNat c) => Samples -> Audio r c
silenceSamples samps = let
  o = Audio $ replicate (fromIntegral $ channels o) $ V.replicate (fromIntegral samps) 0
  in o

silenceSecs :: (KnownNat r, KnownNat c) => Seconds -> Audio r c
silenceSecs secs = let
  o = silenceSamples samps
  samps = floor $ secs * fromIntegral (rate o)
  in o

silent :: (Monad m, KnownNat r, KnownNat c) => Seconds -> C.Source m (Audio r c)
silent secs = let
  (fullSecs, partSecs) = properFraction secs
  fullSec = silenceSamples $ rate fullSec
  partSec = silenceSecs partSecs
  in do
    replicateM_ fullSecs $ C.yield fullSec
    C.yield partSec

merge :: (Monad m) =>
  C.Source m (Audio r c1) -> C.Source m (Audio r c2) -> C.Source m (Audio r (c1 + c2))
merge x y = let
  pairs = combineAudio (x =$= CL.map audioData) (y =$= CL.map audioData)
  mergePair (vs1, vs2) = Audio $ vs1 ++ vs2
  in pairs =$= CL.map mergePair

mix :: (Monad m) =>
  C.Source m (Audio r c) -> C.Source m (Audio r c) -> C.Source m (Audio r c)
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

concatenate :: (Monad m) =>
  C.Source m (Audio r c) -> C.Source m (Audio r c) -> C.Source m (Audio r c)
concatenate = (>>)

gain :: (Monad m) => Float -> C.Conduit (Audio r c) m (Audio r c)
gain d = CL.map $ \(Audio vs) -> Audio $ map (V.map (* d)) vs

dropStart :: (Monad m) => Samples -> C.Conduit (Audio r c) m (Audio r c)
dropStart s = C.await >>= \case
  Nothing         -> return ()
  Just (Audio vs) -> let
    len = fromIntegral $ V.length $ head vs
    in case compare s len of
      EQ -> CL.map id
      LT -> C.yield (Audio $ map (V.drop $ fromIntegral s) vs) >> CL.map id
      GT -> dropStart $ s - len

dropEnd :: (Monad m) => Samples -> C.Conduit (Audio r c) m (Audio r c)
dropEnd = undefined

fadeIn :: (Monad m) => Samples -> C.Conduit (Audio r c) m (Audio r c)
fadeIn = undefined

fadeOut :: (Monad m) => Samples -> C.Conduit (Audio r c) m (Audio r c)
fadeOut = undefined

takeStart :: (Monad m) => Samples -> C.Conduit (Audio r c) m (Audio r c)
takeStart s = C.await >>= \case
  Nothing         -> return ()
  Just (Audio vs) -> let
    len = fromIntegral $ V.length $ head vs
    in case compare s len of
      EQ -> C.yield $ Audio vs
      LT -> C.yield $ Audio $ map (V.take $ fromIntegral s) vs
      GT -> C.yield (Audio vs) >> takeStart (s - len)

takeEnd :: (Monad m) => Samples -> C.Conduit (Audio r c) m (Audio r c)
takeEnd = undefined
