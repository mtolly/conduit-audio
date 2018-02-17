module Data.Conduit.Audio.Mpg123 where

import qualified Codec.Mpg123.Raw             as M
import           Control.Exception            (bracket, bracket_)
import           Control.Monad                (unless)
import           Control.Monad.Fix            (fix)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Bits                    (shiftR)
import qualified Data.Conduit                 as C
import           Data.Conduit.Audio
import qualified Data.Vector.Storable         as V
import           Foreign                      (alloca, castPtr, copyBytes,
                                               finalizerFree, free, mallocBytes,
                                               newForeignPtr, nullPtr, peek)
import           Foreign.C                    (peekCString, withCString)

sourceMpg
  :: (MonadResource m)
  => FilePath
  -> IO (AudioSource m Float)
sourceMpg fin = do
  let check ctx f = f >>= \n -> if M.Mpg123_errors n == M.mpg123_ok
        then return ()
        else M.c_mpg123_plain_strerror n >>= peekCString >>= \err -> error $ ctx ++ ": " ++ err
      checkp ctx f = alloca $ \p -> do
        x <- f p
        check ctx $ peek p
        return x
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
  let src = C.bracketP (checkp "" $ M.c_mpg123_new nullPtr) M.c_mpg123_delete $ \mh -> do
        C.bracketP (check "" $ withCString fin $ M.c_mpg123_open mh) (\() -> check "" $ M.c_mpg123_close mh) $ \() -> do
          bufSize <- liftIO $ do
            check "mpg123_format_none" $ M.c_mpg123_format_none mh
            check "mpg123_format" $ M.c_mpg123_format mh r chans
              $ M.mpg123_enc_enum M.mpg123_enc_float_32
            M.c_mpg123_outblock mh
          liftIO $ check "mpg123_getformat" $ M.c_mpg123_getformat mh nullPtr nullPtr nullPtr
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
                C.yield $ V.unsafeFromForeignPtr0 fptr $ done `shiftR` 2
                loop
  return $ AudioSource src (realToFrac r) (fromIntegral chans) (fromIntegral len)
