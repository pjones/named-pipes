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
module System.NamedPipe.Internal
  ( PipeUse (..),
    ReadEnd,
    WriteEnd,
  )
where

-- | Type for selecting which end of a pipe should be opened.
--
-- @since 0.0.0.0
data PipeUse
  = -- | The reading end of a pipe.
    PipeReadEnd
  | -- | The writing end of a pipe.
    PipeWriteEnd
  deriving (Show, Eq)

-- | Denotes a named pipe that is opened for reading.
--
-- @since 0.0.0.0
type ReadEnd = 'PipeReadEnd

-- | Denotes a named pipe that is opened for writing.
--
-- @since 0.0.0.0
type WriteEnd = 'PipeWriteEnd
