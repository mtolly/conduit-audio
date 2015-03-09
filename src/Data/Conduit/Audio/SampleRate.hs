{-# LANGUAGE LambdaCase #-}
module Data.Conduit.Audio.SampleRate where

import Data.Conduit.Audio
import qualified Data.Conduit.Audio.SampleRate.Binding as SRC
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Vector.Storable as V
import qualified Data.Conduit as C
import Control.Monad.Fix (fix)
import Control.Monad (when)
import Foreign

resample :: (MonadIO m) => Double -> AudioSource m -> AudioSource m
resample rat src = resampleTo (rat * rate src) src

resampleTo :: (MonadIO m) => Rate -> AudioSource m -> AudioSource m
resampleTo r' (AudioSource s r c l) = let
  rat = r' / r
  l' = round $ fromIntegral l * rat
  s' = s C.=$= do
    lsr <- liftIO $ SRC.new SRC.SincBestQuality c
    fix $ \loop -> C.await >>= \case
      Nothing -> return ()
      Just v  -> do
        isEnd <- C.await >>= \case
          Nothing -> return True
          Just v' -> do
            C.leftover v'
            return False
        let inLen  = vectorFrames v c
            outLen = round $ fromIntegral inLen * rat * 1.1
        outPtr <- liftIO $ mallocArray $ outLen * c
        dout <- liftIO $ V.unsafeWith v $ \inPtr -> do
          SRC.process lsr $ SRC.DataIn
            { SRC.data_in       = castPtr inPtr
            , SRC.data_out      = castPtr outPtr
            , SRC.input_frames  = fromIntegral inLen
            , SRC.output_frames = fromIntegral outLen
            , SRC.src_ratio     = rat
            , SRC.end_of_input  = isEnd
            }
        outFP <- liftIO $ newForeignPtr finalizerFree outPtr
        let v' = V.unsafeFromForeignPtr0 outFP $
              fromIntegral (SRC.output_frames_gen dout) * c
        v' `C.yieldOr` liftIO (SRC.delete lsr)
        let inUsed = fromIntegral $ SRC.input_frames_used dout
        when (inUsed /= inLen) $ C.leftover $ V.drop (inUsed * c) v
        loop
    liftIO $ SRC.delete lsr
  in setLengthFrames l' $ AudioSource s' r' c l'
