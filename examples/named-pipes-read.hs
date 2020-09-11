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

import Control.Monad (unless)
import qualified Data.ByteString as ByteString
import System.Environment (getArgs)
import System.IO (stdout)
import System.NamedPipe

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> go file
    _ -> putStrLn "Usage: named-pipe-read <file>"
  where
    go :: FilePath -> IO ()
    go file = do
      exists <- doesNamedPipeExist file
      unless exists (createNamedPipe file)
      withReadEnd file reader

    reader :: NamedPipe ReadEnd -> IO ()
    reader pipe = do
      bs <- namedPipeRead pipe 1024
      if ByteString.null bs
        then pure ()
        else do
          ByteString.hPut stdout bs -- Write out what we read.
          reader pipe -- Try another read.
