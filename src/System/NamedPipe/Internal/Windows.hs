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
module System.NamedPipe.Internal.Windows
  (
  )
where

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
#error "not implemented"
#endif
