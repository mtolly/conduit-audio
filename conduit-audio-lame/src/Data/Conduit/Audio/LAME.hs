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
      o $ L.setNumChannels lame c
      o $ L.setVBR lame L.VbrDefault
      o $ L.initParams lame
      fix $ \loop -> C.await >>= \case
        Nothing -> liftIO $ do
          bs <- allocaArray 7200 $ \buf -> do
            len <- L.encodeFlush lame (castPtr buf) 7200
            B.packCStringLen (buf, len)
          B.hPutStr fout bs
        Just v -> do
          let nsamples = A.vectorFrames v c
              mp3bufsize = ceiling (1.25 * fromIntegral nsamples + 7200 :: Double) :: Int
          liftIO $ V.unsafeWith v $ \p -> do
            bs <- allocaArray mp3bufsize $ \buf -> do
              len <- L.encodeBufferInterleavedIeeeFloat lame (castPtr p) nsamples (castPtr buf) mp3bufsize
              when (len < 0) $ error $ "sinkMP3: negative return value from encode fn: " ++ show len
              B.packCStringLen (buf, len)
            B.hPutStr fout bs
          loop
