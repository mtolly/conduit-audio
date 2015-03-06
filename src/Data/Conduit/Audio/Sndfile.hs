{-# LANGUAGE LambdaCase #-}
module Data.Conduit.Audio.Sndfile where

import Data.Conduit.Audio
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Sound.File.Sndfile as Snd
import qualified Sound.File.Sndfile.Buffer.Vector as SndBuf
import Control.Monad.IO.Class
import Control.Monad (void)
import qualified Data.Vector.Storable as V
import Control.Monad.Fix (fix)

sourceSnd :: (MonadIO m) => FilePath -> C.Source m Audio
sourceSnd fp = do
  h <- liftIO $ Snd.openFile fp Snd.ReadMode Snd.defaultInfo
  let chans = Snd.channels $ Snd.hInfo h
      chunkSize = 10000
  fix $ \loop -> liftIO (Snd.hGetBuffer h chunkSize) >>= \case
    Nothing -> liftIO $ Snd.hClose h
    Just buf -> do
      let a = Audio $ deinterleave (fromIntegral chans) $ SndBuf.fromBuffer buf
      a `C.yieldOr` liftIO (Snd.hClose h)
      loop

sinkSnd :: (MonadIO m) => FilePath -> Snd.Info -> C.Sink Audio m ()
sinkSnd fp info = do
  h <- liftIO $ Snd.openFile fp Snd.WriteMode info
  CL.mapM_ $ \(Audio vs) ->
    liftIO $ void $ Snd.hPutBuffer h $ SndBuf.toBuffer $ interleave vs
  liftIO $ Snd.hClose h

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
