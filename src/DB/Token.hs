{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module DB.Token where

import Import.NoFoundation
import qualified DB.Generics as G
import qualified Data.Time.Clock as T

import qualified Database.SQLite.Simple.ToRow as To
import qualified Database.SQLite.Simple.ToField as ToF
import qualified Database.SQLite.Simple.FromRow as From
import qualified Database.SQLite.Simple.FromField as FromF

data Token = Token
  { _tTokenId :: TokenId
  , _tName :: TokenName
  , _tMaxSize :: Maybe Word64
  , _tExpiresAt :: T.UTCTime
  , _tIsValid :: Bool
  , _tCreatedAt :: T.UTCTime
  , _tDeletedAt :: Maybe T.UTCTime
  }
  deriving stock (Show, Generic)

-- TODO: figure out why the deriving via mechanism doesn't work there
-- deriving From.FromRow via (G.FromRowT Token)
instance From.FromRow Token where
  fromRow = G.unFromRowT <$> From.fromRow

newtype TokenId = TokenId { getTokenId :: Int64 }
  deriving stock (Show)
  deriving newtype (FromF.FromField, ToF.ToField)

data TokenInsert = TokenInsert
  { _tiName :: TokenName
  , _tiMaxSize :: Maybe SizeInByte
  , _tiExpiresAt :: Maybe T.UTCTime
  }
  deriving stock (Show, Generic)
  deriving To.ToRow via (G.ToRowT TokenInsert)

newtype SizeInByte = SizeInByte { getSizeInByte :: Word64 }
  deriving stock (Show)
  deriving ToF.ToField via Word64

newtype TokenName = TokenName { getTokenName :: Text }
  deriving stock (Show, Generic)
  deriving newtype (ToF.ToField, FromF.FromField)

-- data Token = Token
--   { name :: Text
--   , maxSize :: MaxSize
--   , expiresAt :: Expiration
--   }

-- import DB.Common (SizeInByte(..))
--
-- import qualified Data.Time as T
-- import qualified Database.Persist as P
-- import qualified Database.Persist.TH as P.TH
-- import qualified Database.Persist.Sqlite as P.Sqlite
--
-- P.TH.share [P.TH.mkPersist P.TH.sqlSettings, P.TH.mkMigrate "migrateAll"] [P.TH.persistLowerCase|
-- Token
--   value Text
--   createdAt T.UTCTime
--   expiresAt T.UTCTime
--   maxSize SizeInByte Maybe
--   maxExpiresAt T.UTCTime Maybe
-- |]
