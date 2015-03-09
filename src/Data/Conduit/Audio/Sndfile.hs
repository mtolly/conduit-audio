{-# LANGUAGE LambdaCase #-}
module Data.Conduit.Audio.Sndfile
( sourceSnd, sourceSndFrames
, sinkSnd
) where

import Data.Conduit.Audio
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Sound.File.Sndfile as Snd
import qualified Sound.File.Sndfile.Buffer.Vector as SndBuf
import Control.Monad.IO.Class
import Control.Monad (void)
import Control.Monad.Fix (fix)
import Control.Monad.Trans.Resource (MonadResource)

sourceSndFrames :: (MonadResource m) => FilePath -> Frames -> IO (AudioSource m)
sourceSndFrames fp fms = do
  info <- Snd.getFileInfo fp
  let r = fromIntegral $ Snd.samplerate info
      c = Snd.channels   info
      src = C.bracketP
        (Snd.openFile fp Snd.ReadMode Snd.defaultInfo)
        Snd.hClose
        $ \h -> do
          liftIO $ void $ Snd.hSeek h Snd.AbsoluteSeek fms
          fix $ \loop -> liftIO (Snd.hGetBuffer h chunkSize) >>= \case
            Nothing  -> return ()
            Just buf -> do
              C.yield $ SndBuf.fromBuffer buf
              loop
  return $ AudioSource src r c $ Snd.frames info

sourceSnd :: (MonadResource m)
  => FilePath
  -> Seconds
  -- ^ initial position to seek to in the file
  -- (more efficient than using 'dropStart')
  -> IO (AudioSource m)
sourceSnd fp secs = do
  -- TODO: allow user to supply Snd.Format for raw files?
  info <- Snd.getFileInfo fp
  sourceSndFrames fp $ secondsToFrames secs $ fromIntegral $ Snd.samplerate info

sinkSnd :: (MonadResource m) => FilePath -> Snd.Format -> AudioSource m -> m ()
sinkSnd fp fmt (AudioSource s r c _) = s C.$$ C.bracketP
  (Snd.openFile fp Snd.WriteMode $ Snd.defaultInfo
    { Snd.format     = fmt
    , Snd.samplerate = round r
    , Snd.channels   = c
    })
  Snd.hClose
  (\h -> CL.mapM_ $ liftIO . void . Snd.hPutBuffer h . SndBuf.toBuffer)
