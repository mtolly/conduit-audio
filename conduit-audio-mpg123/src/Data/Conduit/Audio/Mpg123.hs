{-# LANGUAGE ScopedTypeVariables #-}
module Data.Conduit.Audio.Mpg123
( sourceMpg
, sourceMpgFrom
, MpgFormat(..)
) where

import qualified Codec.Mpg123.Raw             as M
import           Control.Exception            (bracket, bracket_)
import           Control.Monad                (unless, void)
import           Control.Monad.Fix            (fix)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource (MonadResource)
import qualified Data.Conduit                 as C
import           Data.Conduit.Audio
import           Data.Int
import qualified Data.Vector.Storable         as V
import           Foreign                      hiding (void)
import           Foreign.C                    (peekCString, withCString, CInt(..))

class (Storable a) => MpgFormat a where
  mpgFormat :: a -> M.Mpg123_enc_enum

instance MpgFormat Float where
  mpgFormat _ = M.mpg123_enc_float_32

instance MpgFormat Int8 where
  mpgFormat _ = M.mpg123_enc_signed_8

instance MpgFormat Int16 where
  mpgFormat _ = M.mpg123_enc_signed_16

instance MpgFormat Int32 where
  mpgFormat _ = M.mpg123_enc_signed_32

sourceMpg
  :: forall m a
  .  (MonadResource m, MpgFormat a)
  => FilePath
  -> IO (AudioSource m a)
sourceMpg = sourceMpgFrom $ Frames 0

sourceMpgFrom
  :: forall m a
  .  (MonadResource m, MpgFormat a)
  => Duration
  -> FilePath
  -> IO (AudioSource m a)
sourceMpgFrom pos fin = do
  let check ctx f = f >>= \n -> if M.Mpg123_errors n == M.mpg123_ok
        then return ()
        else M.c_mpg123_plain_strerror n >>= peekCString >>= \err -> error $ ctx ++ ": " ++ err
      checkp ctx f = alloca $ \p -> do
        x <- f p
        check ctx $ peek p
        return x
      fmt = mpgFormat (undefined :: a)
      size = sizeOf (undefined :: a)
  check "mpg123_init" M.c_mpg123_init
  (r, chans, _enc, len) <- bracket (checkp "" $ M.c_mpg123_new nullPtr) M.c_mpg123_delete $ \mh -> do
    bracket_ (check "" $ withCString fin $ M.c_mpg123_open mh) (check "" $ M.c_mpg123_close mh) $ do
      alloca $ \prate -> do
        alloca $ \pchans -> do
          alloca $ \penc -> do
            check "mpg123_getformat" $ M.c_mpg123_getformat mh prate pchans penc
            -- do we need to call seek before length?
            (,,,) <$> peek prate <*> peek pchans <*> peek penc <*> M.c_mpg123_length mh
  -- TODO check that len is non-negative (negative indicates error)
  let seekTo = fromIntegral $ case pos of
        Frames  f -> f
        Seconds s -> secondsToFrames s $ fromIntegral r
      src = C.bracketP (checkp "" $ M.c_mpg123_new nullPtr) M.c_mpg123_delete $ \mh -> do
        C.bracketP (check "" $ withCString fin $ M.c_mpg123_open mh) (\() -> check "" $ M.c_mpg123_close mh) $ \() -> do
          bufSize <- liftIO $ do
            check "mpg123_format_none" $ M.c_mpg123_format_none mh
            check "mpg123_format" $ M.c_mpg123_format mh r chans $ M.mpg123_enc_enum fmt
            M.c_mpg123_outblock mh
          liftIO $ check "mpg123_getformat" $ M.c_mpg123_getformat mh nullPtr nullPtr nullPtr
          liftIO $ void $ haskellGetSeekSet >>= M.c_mpg123_seek mh seekTo
          C.bracketP (mallocBytes $ fromIntegral bufSize) free $ \buf -> do
            fix $ \loop -> do
              (err, done) <- liftIO $ alloca $ \pdone -> do
                err <- M.c_mpg123_read mh buf bufSize pdone
                done <- fromIntegral <$> peek pdone
                return (err, done)
              -- err can be MPG123_OK, MPG123_ERR, MPG123_NEW_FORMAT
              unless (done == 0 || M.Mpg123_errors err /= M.mpg123_ok) $ do
                fptr <- liftIO $ do
                  buf' <- mallocBytes done
                  copyBytes buf' buf done
                  newForeignPtr finalizerFree $ castPtr buf'
                C.yield $ V.unsafeFromForeignPtr0 fptr $ done `quot` size
                loop
  return $ AudioSource src (fromIntegral r) (fromIntegral chans) (fromIntegral len)

foreign import ccall unsafe
  haskellGetSeekSet :: IO CInt
