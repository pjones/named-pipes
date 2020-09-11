-- |
--
-- Copyright:
--   This file is part of the package named-pipes. It is subject
--   to the license terms in the LICENSE file found in the top-level
--   directory of this distribution and at:
--
--     https://github.com/pjones/named-pipes
--
--   No part of this package, including this file, may be copied,
--   modified, propagated, or distributed except according to the terms
--   contained in the LICENSE file.
--
-- License: BSD-2-Clause
module Main
  ( main,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async)
import qualified Control.Concurrent.Async as Async
import qualified Data.ByteString as ByteString
import Pipes
import qualified Pipes.ByteString as PB
import System.NamedPipe

-- | Produce a stream of lines.
readLines ::
  forall m.
  MonadIO m =>
  NamedPipe ReadEnd ->
  Producer ByteString m ()
readLines pipe = PB.concats (PB.splitsWith newline chunks)
  where
    chunks :: Producer ByteString m ()
    chunks = do
      bs <- namedPipeRead pipe 2048
      if ByteString.null bs
        then pure ()
        else yield bs >> chunks
    newline :: Word8 -> Bool
    newline = (==) (fromIntegral $ ord '\n')

-- | The fake work load.
workload :: [Int]
workload = [1 .. 25]

-- | Write some numbers down a pipe.
writerThread :: IO ()
writerThread = do
  putTextLn "writerThread start"
  withWriteEnd "test.fifo" $ \pipe -> do
    putTextLn "writerThread has pipe"
    for_ workload $ \n -> do
      putTextLn ("writerThread at " <> show n)
      namedPipeWrite pipe (show n <> "\n")

-- | Read numbers from a named pipe.
readerThread :: TVar Int -> IO ()
readerThread var = do
  putTextLn "readerThread start"
  withReadEnd "test.fifo" $ \pipe -> do
    putTextLn "readerThread has pipe"
    runEffect (readLines pipe >-> decode)
  where
    decode :: Consumer ByteString IO ()
    decode = do
      bs <- await
      putTextLn ("readerThread has " <> show bs)
      case readMaybe (decodeUtf8 bs) of
        Nothing -> pure ()
        Just n -> atomically (writeTVar var n) >> decode

-- | Do some counting.
counterTread :: TVar Int -> IO ()
counterTread var = do
  putTextLn "counterTread start"
  for_ workload $ \n -> do
    putTextLn ("counterTread at " <> show n)
    atomically (writeTVar var n)
    threadDelay 25000

-- | Run an action that should block and ensure it doesn't block the runtime.
testActionDoesNotBlock :: IO () -> IO ()
testActionDoesNotBlock action = do
  var <- newTVarIO 0
  aid <- async action
  cid <- async (counterTread var)
  Async.wait cid
  Async.cancel aid

  val <- readTVarIO var
  when (Just val /= viaNonEmpty last workload) exitFailure

-- | Main.
main :: IO ()
main = do
  --  Test that a writer with no read doesn't block the entire runtime.
  putTextLn "=======> Solo Writer <======="
  withNamedPipe "test.fifo" (testActionDoesNotBlock writerThread)

  -- Test that a reader with no writer doesn't block the entire runtime.
  putTextLn "=======> Solo Reader <======="
  withNamedPipe "test.fifo" $ testActionDoesNotBlock (newTVarIO 0 >>= readerThread)

  -- Test that a writer and reader can work together.
  putTextLn "=======> Reader + Writer <======="
  withNamedPipe "test.fifo" $ do
    var <- newTVarIO 0
    wid <- async writerThread
    rid <- async (readerThread var)

    Async.wait wid
    Async.wait rid

    val <- readTVarIO var
    when (Just val /= viaNonEmpty last workload) exitFailure
