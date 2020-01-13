{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData #-}

module DB.FileMetadata where

import Import

import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.ToField as SQL.F
import qualified Database.SQLite.Simple.ToRow   as SQL.R

import GHC.Generics
import qualified DB.Generics

newtype SizeInByte = SizeInByte { getSizeInByte :: Word64 }
  deriving stock (Show, Generic)
  deriving SQL.F.ToField via Word64

data FileMetadata = FileMetadata
  { id          :: Int64
  , name        :: Text
  , contentType :: Text
  , size        :: SizeInByte
  , localPath   :: FilePath
  }

  deriving (Show, Generic)

instance SQL.ToRow FileMetadata where
  toRow = DB.Generics.defaultToRow
