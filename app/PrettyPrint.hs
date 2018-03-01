-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  2018 Battelle Memorial Institute
-- License     :  BSD3 (see the LICENSE.txt and WARRANTY.txt files in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

module Main (main) where

import qualified Data.Attoparsec.ByteString.Char8 (parseOnly)
import qualified Data.ByteString (getContents)
import qualified IDF.Parser.ByteString.Char8 (document)
import qualified System.Exit (exitFailure, exitSuccess)
import qualified System.IO (hPutStr, hPrint, stderr, stdout)
import qualified Text.PrettyPrint.Leijen.Text (hPutDoc, pretty)

main :: IO ()
main = do
  source <- Data.ByteString.getContents
  case Data.Attoparsec.ByteString.Char8.parseOnly IDF.Parser.ByteString.Char8.document source of
    Left err -> do
      System.IO.hPutStr System.IO.stderr "Error: "
      System.IO.hPrint System.IO.stderr err
      System.Exit.exitFailure
    Right document -> do
      Text.PrettyPrint.Leijen.Text.hPutDoc System.IO.stdout $ Text.PrettyPrint.Leijen.Text.pretty document
      System.Exit.exitSuccess
