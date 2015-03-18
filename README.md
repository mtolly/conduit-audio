[![Build Status](https://travis-ci.org/mtolly/conduit-audio.svg?branch=master)](https://travis-ci.org/mtolly/conduit-audio)

A suite of Haskell packages that provide a functional, efficient way to work with audio files.

The following sample program uses `libsndfile` to load two audio files,
mixes them together,
resamples to 48 kHz with `libsamplerate`,
and saves the result to an MP3 file with LAME:

    main = do
      src1 <- sourceSnd "file1.flac"
      src2 <- sourceSnd "file2.ogg"
      runResourceT $ sinkMP3 "out.mp3" $
        resampleTo 48000 SincBestQuality $ mix src1 src2

This program runs in constant memory,
by using `conduit` to stream the audio files in small portions.
