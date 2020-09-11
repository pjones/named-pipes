{-# LANGUAGE CPP #-}

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
module System.NamedPipe
  ( -- $use

    -- * Named Pipes
    NamedPipe,

    -- * Creating and Cleaning Up Named Pipes
    doesNamedPipeExist,
    createNamedPipe,
    removeNamedPipe,

    -- * Opening and Closing Named Pipes
    ReadEnd,
    WriteEnd,
    openReadEnd,
    openWriteEnd,
    closeNamedPipe,
    withReadEnd,
    withWriteEnd,

    -- * I/O with Named Pipes
    namedPipeRead,
    namedPipeWrite,

    -- * Utilities
    withNamedPipe,
  )
where

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import System.NamedPipe.Internal.Windows
#else
import System.NamedPipe.Internal.Posix hiding (withNamedPipe)
#endif

import Control.Exception.Safe (MonadMask, finally)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import System.NamedPipe.Internal

-- | Create a named pipe, run the given action, then remove the named pipe.
--
-- @since 0.0.0.0
withNamedPipe ::
  MonadIO m =>
  MonadMask m =>
  FilePath ->
  m a ->
  m a
withNamedPipe file action = do
  exist <- doesNamedPipeExist file
  unless exist (createNamedPipe file)
  finally action (removeNamedPipe file)

-- $use
--
-- A named pipe is a unidirectional IPC channel where one process
-- writes down the pipe and another process reads from the pipe.  On
-- POSIX systems the named pipe shows up in the file system so existing
-- tooling can use the pipe like any other file.
--
-- == Writing to a Named Pipe
--
-- To open a pipe for writing use 'withWriteEnd' followed by
-- 'namedPipeWrite'.  Note that the current thread will block until a
-- reader is attached to the other end of the pipe.  Writing on the
-- pipe may also block if the pipe is full.
--
-- >>> withWriteEnd "test.fifo" $ \pipe -> namedPipeWrite pipe "Hello"
--
-- == Reading from a Named Pipe
--
-- To open a pipe for reading use 'withReadEnd' followed by
-- 'namedPipeRead'.  Note that the current thread will block until a
-- writer is attached to the other end of the pipe or if a writer
-- hasn't written anything yet.
--
-- >>> createNamedPipe "test.fifo" -- Create it if it doesn't exist.
-- >>> withReadEnd "test.fifo" $ \pipe -> namedPipeRead pipe 1020
