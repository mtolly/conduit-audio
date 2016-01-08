{- |
Uses the LAME library to write audio streams to MP3 files.
-}
module Data.Conduit.Audio.LAME where

import qualified Data.Conduit.Audio as A
import qualified Data.Conduit.Audio.LAME.Binding as L
import qualified Data.Conduit as C
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Control.Monad (when, unless)
import Control.Monad.Fix (fix)
import qualified Data.Vector.Storable as V
import qualified Data.ByteString as B
import Foreign
import qualified System.IO as IO
import Control.Monad.Trans.Class (lift)

-- | Saves an audio stream to an MP3 file. Uses the default VBR mode.
sinkMP3 :: (MonadResource m) => FilePath -> A.AudioSource m Float -> m ()
sinkMP3 fp = sinkMP3WithHandle fp $ \lame -> liftIO $ L.check $ L.setVBR lame L.VbrDefault

-- | Lets you perform arbitrary setup on the `lame` handle before encoding.
-- You must select a quality somewhere in the setup action,
-- such as with 'L.setVBR'.
sinkMP3WithHandle :: (MonadResource m) => FilePath -> (L.LAME -> m ()) -> A.AudioSource m Float -> m ()
sinkMP3WithHandle fp setup (A.AudioSource s r c _) = (C.$$) s
  $ C.bracketP L.init (L.check . L.close)
  $ \lame -> C.bracketP (IO.openBinaryFile fp IO.WriteMode) IO.hClose
    $ \fout -> do
      let o = liftIO . L.check
      o $ L.setInSamplerate lame $ round r
      unless (c `elem ` [1, 2]) $ err $
        "only 1 or 2 channels are supported (" ++ show c ++ " given)"
      o $ L.setNumChannels lame c
      lift $ setup lame
      o $ L.initParams lame
      fix $ \loop -> C.await >>= \mx -> case mx of
        Nothing -> liftIO $ do
          bs <- allocaArray 7200 $ \buf -> do
            len <- L.encodeFlush lame (castPtr buf) 7200
            B.packCStringLen (buf, len)
          B.hPutStr fout bs
          IO.hSeek fout IO.AbsoluteSeek 0
          tags <- allocaArray 100000 $ \buf -> do
            len <- L.getLametagFrame lame (castPtr buf) 100000
            when (len < 0) $ err "couldn't get lame tag frame (buffer too small)"
            B.packCStringLen (buf, fromIntegral len)
          B.hPutStr fout tags
        Just v -> do
          let nsamples = A.vectorFrames v c
              mp3bufsize = ceiling (1.25 * fromIntegral nsamples + 7200 :: Double) :: Int
          liftIO $ V.unsafeWith v $ \p -> do
            bs <- allocaArray mp3bufsize $ \buf -> do
              len <- case c of
                1 -> L.encodeBufferIeeeFloat lame (castPtr p) nullPtr nsamples (castPtr buf) mp3bufsize
                _ -> L.encodeBufferInterleavedIeeeFloat lame (castPtr p) nsamples (castPtr buf) mp3bufsize
              when (len < 0) $ err $
                "encode function returned " ++ show len ++ "; " ++ case len of
                  -1 -> "mp3buf was too small"
                  -2 -> "malloc() problem"
                  -3 -> "lame_init_params() not called"
                  -4 -> "psycho acoustic problems"
                  _  -> "unknown error"
              B.packCStringLen (buf, len)
            B.hPutStr fout bs
          loop
  where err = error . ("Conduit.Audio.LAME.sinkMP3WithHandle: " ++)
