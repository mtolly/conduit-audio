module Data.Conduit.Audio.AO where

import qualified Data.Conduit as C
import Data.Conduit.Audio
import qualified Data.Conduit.Audio.AO.Binding as AO
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Foreign hiding (void)
import qualified Data.Vector.Storable as V
import Control.Monad (void)
import Control.Exception (bracket_)

withAO :: IO a -> IO a
withAO = bracket_ AO.initialize AO.shutdown

playSource :: (MonadResource m) => AudioSource m Int16 -> m ()
playSource src = source src C.$$ getDevice where
  getDevice = do
    devID <- liftIO AO.defaultDriverID
    let fmt = AO.SampleFormat
          { AO.bits = 16
          , AO.rate = round $ rate src
          , AO.channels = fromIntegral $ channels src
          , AO.byteFormat = AO.AO_FMT_NATIVE
          , AO.matrix = Nothing
          }
        start = AO.withSampleFormat fmt $ \pfmt -> AO.openLive devID pfmt nullPtr
    C.bracketP start (void . AO.close) useDevice
  useDevice dev = C.await >>= \res -> case res of
    Nothing -> return ()
    Just v  -> do
      let size = fromIntegral $ V.length v * sizeOf (V.head v)
      _ <- liftIO $ V.unsafeWith v $ \p -> AO.play dev (castPtr p) size
      useDevice dev
