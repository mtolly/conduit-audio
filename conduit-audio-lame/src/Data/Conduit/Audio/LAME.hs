{-# LANGUAGE LambdaCase #-}
module Data.Conduit.Audio.LAME where

import qualified Data.Conduit.Audio as A
import qualified Data.Conduit.Audio.LAME.Binding as L
import qualified Data.Conduit as C
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Control.Monad (when)
import Control.Monad.Fix (fix)
import qualified Data.Vector.Storable as V
import qualified Data.ByteString as B
import Foreign
import qualified System.IO as IO

sinkMP3 :: (MonadResource m) => FilePath -> A.AudioSource m Float -> m ()
sinkMP3 fp (A.AudioSource s r c _) = (C.$$) s
  $ C.bracketP L.init (L.check . L.close)
  $ \lame -> C.bracketP (IO.openBinaryFile fp IO.WriteMode) IO.hClose
    $ \fout -> do
      let o = liftIO . L.check
      o $ L.setInSamplerate lame $ round r
      o $ L.setVBR lame L.VbrDefault
      o $ L.initParams lame
      when (c /= 2) $ error "sinkMP3: only stereo supported"
      fix $ \loop -> C.await >>= \case
        Nothing -> liftIO $ do
          buf <- mallocArray 7200
          len <- L.encodeFlush lame buf 7200
          bs <- B.packCStringLen (castPtr buf, len)
          free buf
          B.hPutStr fout bs
        Just v -> do
          let nsamples = A.vectorFrames v c
              mp3bufsize = ceiling (1.25 * fromIntegral nsamples + 7200 :: Double) :: Int
          liftIO $ V.unsafeWith v $ \p -> do
            buf <- mallocArray mp3bufsize
            len <- L.encodeBufferInterleavedIeeeFloat lame (castPtr p) nsamples buf mp3bufsize
            when (len < 0) $ error $ "sinkMP3: negative return value from encode fn: " ++ show len
            bs <- B.packCStringLen (castPtr p, len)
            free buf
            B.hPutStr fout bs
          loop
