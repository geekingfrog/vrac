{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData #-}

module DB.FileMetadata where

import Import.NoFoundation

import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.ToField as ToF
-- import qualified Database.SQLite.Simple.ToRow   as SQL.R
import qualified Database.SQLite.Simple.FromRow   as FromR
import qualified Database.SQLite.Simple.FromField   as FromF
import qualified Database.SQLite.Simple.Ok as Ok
import qualified Data.Text as Tx

import qualified DB.Generics
import qualified Data.UUID as UUID
import qualified DB.Token as Tok

-- newtype SizeInByte = SizeInByte { getSizeInByte :: Word64 }
--   deriving stock (Show, Generic)
--   deriving ToF.ToField via Word64

data FileMetadata = FileMetadata
  { id          :: MetadataId
  , name        :: Text
  , contentType :: Text
  -- , size        :: SizeInByte
  , localPath   :: FilePath
  , tokenId     :: Tok.TokenId
  }

  deriving (Show, Generic)

instance SQL.ToRow FileMetadata where
  toRow = DB.Generics.defaultToRow

instance FromR.FromRow FileMetadata where
  fromRow = DB.Generics.unFromRowT <$> FromR.fromRow

newtype MetadataId = MetadataId { getMetadataId :: UUID.UUID }
  deriving stock (Show)

instance ToF.ToField MetadataId where
  toField (MetadataId uuid) = ToF.toField (UUID.toText uuid)

instance FromF.FromField MetadataId where
  fromField f = do
    text <- FromF.fromField f
    case UUID.fromText text of
      Nothing -> FromF.returnError FromF.ConversionFailed f ("Couldn't parse UUID field: " <> Tx.unpack text)
      Just uuid -> Ok.Ok (MetadataId uuid)
