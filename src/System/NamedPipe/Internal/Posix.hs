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
--
-- Named pipes for operating systems that implement POSIX.
module System.NamedPipe.Internal.Posix
  ( doesNamedPipeExist,
    createNamedPipe,
    removeNamedPipe,
    NamedPipe (..),
    openNamedPipe,
    openWriteEnd,
    openReadEnd,
    closeNamedPipe,
    withNamedPipe,
    withWriteEnd,
    withReadEnd,
    namedPipeRead,
    namedPipeWrite,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception.Safe (MonadMask, bracket, throwIO, tryIO)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Internal as ByteString
import qualified Data.ByteString.Unsafe as UByteString
import Data.Function ((&))
import Data.IORef
import qualified Foreign
import qualified Foreign.C.Error as Errno
import GHC.Conc (closeFdWith, threadWaitRead, threadWaitWrite)
import System.IO (IOMode (ReadMode), openFile)
import qualified System.IO.Error as Error
import System.NamedPipe.Internal
import qualified System.Posix as Posix

-- | 'True' if the given 'FilePath' exists and is a named pipe.
--
-- @since 0.0.0.0
doesNamedPipeExist :: MonadIO m => FilePath -> m Bool
doesNamedPipeExist file = liftIO $ do
  exist <- Posix.fileExist file
  if exist
    then Posix.isNamedPipe <$> Posix.getFileStatus file
    else pure False

-- | Create a named pipe in the file system at the given file path.
--
-- NOTE: the file is created with file mode @0620@.
--
-- @since 0.0.0.0
createNamedPipe :: MonadIO m => FilePath -> m ()
createNamedPipe file = do
  exist <- doesNamedPipeExist file
  unless exist $ liftIO (Posix.createNamedPipe file 0O620)

-- | Remove a named pipe from the file system.
--
-- NOTE: The give file will only be removed if it exists and is a
-- named pipe.
--
-- @since 0.0.0.0
removeNamedPipe :: MonadIO m => FilePath -> m ()
removeNamedPipe file = liftIO $ do
  exist <- doesNamedPipeExist file
  when exist (Posix.removeLink file)

-- | A named pipe is an IPC mechanism that can be opened either as a
-- reader or a writer.  Bytes written on the 'WriteEnd' will show up
-- on the 'ReadEnd'.
--
-- @since 0.0.0.0
data NamedPipe (end :: PipeUse) = NamedPipe
  { -- | The file backing this named pipe.
    namedPipeFile :: !FilePath,
    -- | The file descriptor which can be used for reading from or
    -- writing to the named pipe.
    namedPipeFd :: !Posix.Fd,
    -- | Keep track of EOF.
    namedPipeEOF :: !(IORef Bool)
  }

-- | Open a named pipe.
--
-- For documentation see 'openReadEnd' and 'openWriteEnd'.
--
-- Consider using 'withNamedPipe' if possible.
--
-- @since 0.0.0.0
openNamedPipe ::
  MonadIO m =>
  PipeUse ->
  FilePath ->
  m (NamedPipe end)
openNamedPipe end file = liftIO $ do
  fd <- case end of
    PipeReadEnd -> do
      -- NOTE: openFile does some magic that openFd doesn't (or can't)
      -- do for the reading end.  Using openFd on the reader end will
      -- result in an exception being thrown when a read is attempted.
      h <- openFile file ReadMode
      Posix.handleToFd h
    PipeWriteEnd -> do
      let flags =
            Posix.defaultFileFlags
              { Posix.nonBlock = True,
                Posix.noctty = True
              }
      -- NOTE: The file /must/ be opened non-blocking or the entire
      -- RTS will lock up (the multi-threaded RTS can execute other
      -- threads but the current thread will block and cannot be
      -- cancled).  However, if there are no readers waiting on the
      -- pipe, opening the file non-blocking will throw an exception.
      -- As a kludge we open non-blocking but catch the exception and
      -- keep trying to open the file until we succeed.
      createNamedPipe file
      retryOnErrno Errno.eNXIO (Posix.openFd file Posix.WriteOnly Nothing flags)

  eof <- newIORef False
  pure (NamedPipe file fd eof)

-- | Open the 'WriteEnd' of a named pipe.
--
--   * If the named pipe doesn't exist it will be created with 'createNamedPipe'.
--
--   * If there are no readers attached to the named pipe the current
--     thread will block until they arrive.
--
-- @since 0.0.0.0
openWriteEnd ::
  MonadIO m =>
  FilePath ->
  m (NamedPipe WriteEnd)
openWriteEnd = openNamedPipe PipeWriteEnd

-- | Open the 'ReadEnd' of a named pipe.
--
--   * The named pipe must already exist in the file system.  Use
--     'createNamedPipe' if necessary.
--
-- @since 0.0.0.0
openReadEnd ::
  MonadIO m =>
  FilePath ->
  m (NamedPipe ReadEnd)
openReadEnd = openNamedPipe PipeReadEnd

-- | Close a named pipe.
--
-- Consider using 'withReadEnd' or 'withWriteEnd' if possible.
--
-- This action does not remove the named pipe from the file system.
-- To do that use 'removeNamedPipe'.
--
-- @since 0.0.0.0
closeNamedPipe ::
  MonadIO m =>
  NamedPipe end ->
  m ()
closeNamedPipe NamedPipe {namedPipeFd = fd} =
  liftIO (closeFdWith Posix.closeFd fd)

-- | Safe version of 'openNamedPipe' and 'closeNamedPipe' that
-- automatically closes the named pipe after the given action
-- completes.
--
-- @since 0.0.0.0
withNamedPipe ::
  MonadIO m =>
  MonadMask m =>
  PipeUse ->
  FilePath ->
  (NamedPipe end -> m a) ->
  m a
withNamedPipe mode file =
  bracket (openNamedPipe mode file) closeNamedPipe

-- | Safe version of 'openWriteEnd' that always calls 'closeNamedPipe'.
--
-- @since 0.0.0.0
withWriteEnd ::
  MonadIO m =>
  MonadMask m =>
  FilePath ->
  (NamedPipe WriteEnd -> m a) ->
  m a
withWriteEnd file =
  bracket (openWriteEnd file) closeNamedPipe

-- | Safe version of 'openReadEnd' that always calls 'closeNamedPipe'.
--
-- @since 0.0.0.0
withReadEnd ::
  MonadIO m =>
  MonadMask m =>
  FilePath ->
  (NamedPipe ReadEnd -> m a) ->
  m a
withReadEnd file =
  bracket (openReadEnd file) closeNamedPipe

-- | Read bytes from the given named pipe.
--
-- Be aware that:
--
--   * Reading from a named pipe will block the current thread if
--     there are no writers or no data are available for reading.
--
--   * If the returned 'ByteString' is @null@ (zero bytes) then the
--     'WriteEnd' of the pipe has been closed.  Attempts to read from the
--     pipe after the 'WriteEnd' is closed will throw an exception.
--
-- @since 0.0.0.0
namedPipeRead ::
  MonadIO m =>
  -- | A named pipe opened for reading, usually via 'withReadEnd'.
  NamedPipe ReadEnd ->
  -- | The maximum number of bytes to read.  The returned number of
  -- bytes may be less than requested.
  Int ->
  -- | The bytes that were read.  If the returned 'ByteString' is
  -- empty then the 'WriteEnd' of the named pipe has been closed and
  -- 'namedPipeRead' can no longer be called on this named pipe.
  m ByteString
namedPipeRead pipe@NamedPipe {..} bufsize = liftIO $ do
  eof <- readIORef namedPipeEOF
  when eof (throwEOF pipe "namedPipeRead")
  Foreign.allocaBytes bufsize $ \ptr -> do
    threadWaitRead namedPipeFd
    n <- Posix.fdReadBuf namedPipeFd ptr (fromIntegral bufsize)
    if n == 0
      then writeIORef namedPipeEOF True >> pure ByteString.empty
      else UByteString.unsafePackCStringFinalizer ptr (fromIntegral n) (pure ())

-- | Write bytes to the given named pipe.
--
--  Be aware that:
--
--    * Writing to a named pipe may block the current thread if there
--      are no readers or the pipe buffer is full.
--
-- @since 0.0.0.0
namedPipeWrite ::
  MonadIO m =>
  -- | A named pipe opened for writing, usually via 'withWriteEnd'.
  NamedPipe WriteEnd ->
  -- | The bytes to write down the pipe.
  ByteString ->
  -- | An action that continues writing until all bytes have been sent
  -- down the named pipe.
  m ()
namedPipeWrite NamedPipe {namedPipeFd} = liftIO . go
  where
    go :: ByteString -> IO ()
    go bytes = do
      let (fptr, offset, len) = ByteString.toForeignPtr bytes
      n <- fmap fromIntegral $
        Foreign.withForeignPtr fptr $ \ptr -> do
          threadWaitWrite namedPipeFd
          Posix.fdWriteBuf namedPipeFd (Foreign.plusPtr ptr offset) (fromIntegral len)
      if n == len
        then pure ()
        else go (ByteString.drop n bytes)

-- | Internal function to throw an EOF exception.
--
-- @since 0.0.0.0
throwEOF :: NamedPipe end -> String -> IO ()
throwEOF NamedPipe {..} loc =
  Error.mkIOError Error.eofErrorType loc Nothing (Just namedPipeFile)
    & throwIO

-- | Run an IO action.  If it throws an exception and the current
-- errno matches the given errno, retry the action.
--
-- @since 0.0.0.0
retryOnErrno :: Errno.Errno -> IO a -> IO a
retryOnErrno (Errno.Errno code) action = go
  where
    go =
      tryIO action >>= \case
        Right a -> pure a
        Left e -> do
          Errno.Errno errno <- Errno.getErrno
          -- NOTE: sleeping for 10 milliseconds is totally arbitrary.
          -- However, if this value is too small then for some wild
          -- reason we get memory corruption.
          if errno == code
            then threadDelay 10000 >> go
            else throwIO e
