{-# LANGUAGE LambdaCase #-}
module Data.Conduit.Audio.Sndfile where

import Data.Conduit.Audio
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Sound.File.Sndfile as Snd
import qualified Sound.File.Sndfile.Buffer.Vector as SndBuf
import Control.Monad.IO.Class
import GHC.TypeLits
import Control.Monad (when, void)
import qualified Data.Vector.Storable as V
import Control.Monad.Fix (fix)

sourceSnd :: (MonadIO m, KnownNat r, KnownNat c) =>
  FilePath -> C.Source m (Audio r c)
sourceSnd fp = let
  fakeAudio :: C.Source m (Audio r c) -> Audio r c
  fakeAudio = undefined -- used as proxy
  r = rate     $ fakeAudio source
  c = channels $ fakeAudio source
  source = do
    h <- liftIO $ Snd.openFile fp Snd.ReadMode Snd.defaultInfo
    let info = Snd.hInfo h
    when (r /= fromIntegral (Snd.samplerate info)) $
      error "sourceSnd: incorrect sample rate"
    when (c /= fromIntegral (Snd.channels info)) $
      error "sourceSnd: incorrect number of channels"
    fix $ \loop -> liftIO (Snd.hGetBuffer h chunkSize) >>= \case
      Nothing -> liftIO $ Snd.hClose h
      Just buf -> do
        let a = Audio $ deinterleave (fromIntegral c) $ SndBuf.fromBuffer buf
        a `C.yieldOr` liftIO (Snd.hClose h)
        loop
  chunkSize = 10000
  in source

sinkSnd :: (MonadIO m, KnownNat r, KnownNat c) =>
  FilePath -> Snd.Format -> C.Sink (Audio r c) m ()
sinkSnd fp fmt = let
  fakeAudio :: C.Sink (Audio r c) m () -> Audio r c
  fakeAudio = undefined -- used as proxy
  sink = do
    let info = Snd.defaultInfo
          { Snd.format     = fmt
          , Snd.samplerate = fromIntegral $ rate     $ fakeAudio sink
          , Snd.channels   = fromIntegral $ channels $ fakeAudio sink
          }
    h <- liftIO $ Snd.openFile fp Snd.WriteMode info
    CL.mapM_ $ \(Audio vs) ->
      liftIO $ void $ Snd.hPutBuffer h $ SndBuf.toBuffer $ interleave vs
    liftIO $ Snd.hClose h
  in sink

-- | Given a vector with interleaved samples, like @[L0, R0, L1, R1, ...]@,
-- converts it into @[[L0, L1, ...], [R0, R1, ...]]@.
deinterleave :: (V.Storable a)
  => Int -- ^ The number of channels to split into.
  -> V.Vector a
  -> [V.Vector a]
deinterleave n v = do
  let len = V.length v `div` n
  i <- [0 .. n - 1]
  return $ V.generate len $ \j -> v V.! (n * j + i)

-- | All the vectors must be the same length; otherwise behavior is undefined.
interleave :: (V.Storable a) => [V.Vector a] -> V.Vector a
interleave vs = let
  n = length vs
  in V.generate (sum $ map V.length vs) $ \i -> let
    (q, r) = quotRem i n
    in (vs !! r) V.! q
