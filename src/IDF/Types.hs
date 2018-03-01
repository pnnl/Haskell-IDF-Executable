-----------------------------------------------------------------------------
-- |
-- Module      :  IDF.Types
-- Copyright   :  2018 Battelle Memorial Institute
-- License     :  BSD3 (see the LICENSE.txt and WARRANTY.txt files in the distribution)
--
-- Maintainer  :  mark.borkum@pnnl.gov
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-}

module IDF.Types where

import qualified Data.List (intersperse)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty (toList)
import           Data.Scientific (Scientific)
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text.Lazy (fromStrict, pack)
import           Text.PrettyPrint.Leijen.Text (Pretty(pretty), (<+>))
import qualified Text.PrettyPrint.Leijen.Text (char, empty, hcat, text, vcat)

data Comment = Comment
  { commentRetention :: Retention
  , commentData :: Text
  } deriving (Eq, Ord, Read, Show)

data Document = Document
  { documentRoot :: Element
  } deriving (Eq, Ord, Read, Show)

data Element = Element
  { elementObjects :: [Object]
  } deriving (Eq, Ord, Read, Show)

data Name = Name
  { nameSegments :: NonEmpty Text
  } deriving (Eq, Ord, Read, Show)

data Object = Object
  { objectName :: Name
  , objectValues :: [Value]
  } deriving (Eq, Ord, Read, Show)

data Retention = NotRetained | Retained
  deriving (Eq, Ord, Read, Show)

data Value = NoValue | Number Scientific | String Text | Reference Name
  deriving (Eq, Ord, Read, Show)

cCOMMENT_PREFIX :: Char
cCOMMENT_PREFIX = '!'

cCOMMENT_RETENTION_PREFIX :: Char
cCOMMENT_RETENTION_PREFIX = '-'

cNAME_SEPARATOR :: Char
cNAME_SEPARATOR = ':'

cNODE_SEPARATOR :: Char
cNODE_SEPARATOR = ';'

cVALUE_SEPARATOR :: Char
cVALUE_SEPARATOR = ','

instance Pretty Comment where
  pretty Comment{..} = Text.PrettyPrint.Leijen.Text.char cCOMMENT_PREFIX <> pretty commentRetention <+> Text.PrettyPrint.Leijen.Text.text (Data.Text.Lazy.fromStrict commentData)

instance Pretty Document where
  pretty Document{..} = pretty documentRoot

instance Pretty Element where
  pretty Element{..} = Text.PrettyPrint.Leijen.Text.vcat (map pretty elementObjects)

instance Pretty Name where
  pretty Name{..} = Text.PrettyPrint.Leijen.Text.hcat (Data.List.intersperse (Text.PrettyPrint.Leijen.Text.char cNAME_SEPARATOR) (map (Text.PrettyPrint.Leijen.Text.text . Data.Text.Lazy.fromStrict) (Data.List.NonEmpty.toList nameSegments)))

instance Pretty Object where
  pretty Object{..} = pretty objectName <> Text.PrettyPrint.Leijen.Text.char cVALUE_SEPARATOR <> Text.PrettyPrint.Leijen.Text.hcat (Data.List.intersperse (Text.PrettyPrint.Leijen.Text.char cVALUE_SEPARATOR) (map pretty objectValues)) <> Text.PrettyPrint.Leijen.Text.char cNODE_SEPARATOR

instance Pretty Retention where
  pretty NotRetained = Text.PrettyPrint.Leijen.Text.char cCOMMENT_RETENTION_PREFIX
  pretty Retained = Text.PrettyPrint.Leijen.Text.empty

instance Pretty Value where
  pretty NoValue = Text.PrettyPrint.Leijen.Text.empty
  pretty (Number x) = Text.PrettyPrint.Leijen.Text.text (Data.Text.Lazy.pack (show x))
  pretty (String x) = Text.PrettyPrint.Leijen.Text.text (Data.Text.Lazy.fromStrict x)
  pretty (Reference x) = pretty x
