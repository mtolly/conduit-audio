{- |
Example app demonstrating the conduit-audio interfaces.
Run @./test in.xyz out.mp3@ to convert @in.xyz@ to a 48 kHz MP3.
-}
module Main where

import Data.Conduit.Audio.Sndfile
import Data.Conduit.Audio.SampleRate
import Data.Conduit.Audio.LAME
import System.Environment (getArgs)
import Control.Monad.Trans.Resource (runResourceT)

main :: IO ()
main = do
  [fin, fout] <- getArgs
  src <- sourceSnd fin
  runResourceT $ sinkMP3 fout $ resampleTo 48000 SincBestQuality src
