{-# LANGUAGE LambdaCase #-}
module Data.Conduit.Audio.SampleRate where

import Data.Conduit.Audio
import qualified Data.Conduit.Audio.SampleRate.Binding as SRC
import Control.Monad.IO.Class (liftIO)
import qualified Data.Vector.Storable as V
import qualified Data.Conduit as C
import Control.Monad.Fix (fix)
import Control.Monad (when)
import Foreign
import Control.Monad.Trans.Resource (MonadResource)

resample
  :: (MonadResource m)
  => Double -- ^ the ratio of new sample rate to old sample rate
  -> AudioSource m
  -> AudioSource m
resample rat src = resampleTo (rat * rate src) src

resampleTo
  :: (MonadResource m) => Rate -> AudioSource m -> AudioSource m
resampleTo r' (AudioSource s r c l) = let
  rat = r' / r
  l' = round $ fromIntegral l * rat
  s' = s C.=$= C.bracketP
    (SRC.new SRC.SincBestQuality c)
    SRC.delete
    (\lsr -> fix $ \loop -> C.await >>= \case
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
        when (V.length v' /= 0) $ C.yield v'
        let inUsed = fromIntegral $ SRC.input_frames_used dout
        when (inUsed /= inLen) $
          -- we want to make sure we don't keep giving SRC too small a chunk forever
          if SRC.output_frames_gen dout /= 0
            then C.leftover $ V.drop (inUsed * c) v
            -- ^ SRC did produce some output this time, so we're fine
            else C.await >>= \case
              Nothing -> return ()
              -- ^ this should never happen, right?
              -- that would mean v was the last chunk, we told SRC it was the
              -- last chunk, but then it didn't use it all
              Just v' -> C.leftover $ V.drop (inUsed * c) v V.++ v'
              -- ^ if v was too small to produce anything,
              -- glue it onto v' to make a bigger chunk
        loop
    )
  in AudioSource s' r' c l'
