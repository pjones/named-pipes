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

import qualified Data.ByteString.Char8 as ByteString
import Data.Foldable (for_)
import System.Environment (getArgs, getEnvironment)
import System.NamedPipe

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> withWriteEnd file writer
    _ -> putStrLn "Usage: named-pipe-write <file>"
  where
    writer :: NamedPipe WriteEnd -> IO ()
    writer pipe = do
      env <- getEnvironment
      for_ env $ \(key, value) ->
        namedPipeWrite
          pipe
          ( ByteString.pack key <> "="
              <> ByteString.pack value
              <> "\n"
          )
