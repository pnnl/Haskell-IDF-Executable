-----------------------------------------------------------------------------
-- |
-- Module      :  IDF.Parser.ByteString.Char8
-- Copyright   :  2018 Battelle Memorial Institute
-- License     :  BSD3 (see the LICENSE.txt and WARRANTY.txt files in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

module IDF.Parser.ByteString.Char8 where

import           Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 (anyChar, char, endOfLine, inClass, isAlpha_ascii, isDigit, many', manyTill, option, parseOnly, sepBy', skipSpace, space, scientific, takeTill, takeWhile1, try)
import qualified Data.ByteString (null)
import           Data.Functor (void)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Set (Set)
import qualified Data.Set (fromList, member)
import qualified Data.Text (pack, strip)
import qualified Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           IDF.Types

comment :: Parser Comment
comment = do
  void $ Data.Attoparsec.ByteString.Char8.char cCOMMENT_PREFIX
  new_commentRetention <- Data.Attoparsec.ByteString.Char8.option Retained $ NotRetained <$ Data.Attoparsec.ByteString.Char8.char cCOMMENT_RETENTION_PREFIX
  new_commentData <- Data.Text.pack <$> (Data.Attoparsec.ByteString.Char8.manyTill Data.Attoparsec.ByteString.Char8.anyChar $ Data.Attoparsec.ByteString.Char8.try Data.Attoparsec.ByteString.Char8.endOfLine)
  pure Comment
    { commentRetention = new_commentRetention
    , commentData = new_commentData
    }

comments :: Parser [Comment]
comments = do
  Data.Attoparsec.ByteString.Char8.skipSpace
  new_comments <- Data.Attoparsec.ByteString.Char8.sepBy' comment $ Data.Attoparsec.ByteString.Char8.many' Data.Attoparsec.ByteString.Char8.space
  Data.Attoparsec.ByteString.Char8.skipSpace
  pure new_comments

document :: Parser Document
document = do
  new_documentRoot <- element
  void comments
  pure Document
    { documentRoot = new_documentRoot
    }

element :: Parser Element
element = do
  new_elementObjects <- Data.Attoparsec.ByteString.Char8.many' $ do
    void comments
    object
  pure Element
    { elementObjects = resolveReferences new_elementObjects
    }

resolveReferences :: [Object] -> [Object]
resolveReferences objects0 = map (\object0 -> object0 { objectValues = map f $ objectValues object0 }) objects0
  where
    names0 :: Set Name
    names0 = Data.Set.fromList $ map objectName objects0
    f :: Value -> Value
    f x@(String xs)
      | (Right name0) <- Data.Attoparsec.ByteString.Char8.parseOnly name $ Data.Text.Encoding.encodeUtf8 xs, name0 `Data.Set.member` names0 = Reference name0
      | otherwise = x
    f x = x

name :: Parser Name
name = do
  new_nameSegments_head <- Data.Text.Encoding.decodeUtf8 <$> Data.Attoparsec.ByteString.Char8.takeWhile1 isName_ascii
  new_nameSegments_tail <- Data.Attoparsec.ByteString.Char8.many' $ do
    void $ Data.Attoparsec.ByteString.Char8.char cNAME_SEPARATOR
    Data.Text.Encoding.decodeUtf8 <$> Data.Attoparsec.ByteString.Char8.takeWhile1 isName_ascii
  pure Name
    { nameSegments = new_nameSegments_head :| new_nameSegments_tail
    }

object :: Parser Object
object = do
  new_objectName <- name
  new_objectValues <- Data.Attoparsec.ByteString.Char8.many' $ do
    void comments
    void $ Data.Attoparsec.ByteString.Char8.char cVALUE_SEPARATOR
    void comments
    value
  void comments
  void $ Data.Attoparsec.ByteString.Char8.char cNODE_SEPARATOR
  pure Object
    { objectName = new_objectName
    , objectValues = new_objectValues
    }

value :: Parser Value
value = do
  xs <- Data.Attoparsec.ByteString.Char8.takeTill $ Data.Attoparsec.ByteString.Char8.inClass [cVALUE_SEPARATOR, cNODE_SEPARATOR]
  if Data.ByteString.null xs
    then do
      pure NoValue
    else case Data.Attoparsec.ByteString.Char8.parseOnly Data.Attoparsec.ByteString.Char8.scientific xs of
      Left _ -> pure $ String $ Data.Text.strip $ Data.Text.Encoding.decodeUtf8 xs
      Right n -> pure $ Number n

isName_ascii :: Char -> Bool
isName_ascii = pure (||) <*> Data.Attoparsec.ByteString.Char8.isDigit <*> Data.Attoparsec.ByteString.Char8.isAlpha_ascii
