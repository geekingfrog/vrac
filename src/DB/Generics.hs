{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module DB.Generics where

import GHC.Generics

import qualified Database.SQLite.Simple         as SQL
import qualified Database.SQLite.Simple.ToField as SQL.F
import qualified Database.SQLite.Simple.ToRow as To
import qualified Database.SQLite.Simple.FromRow as From
import qualified Database.SQLite.Simple.FromField as FromF


class GToRow1 f where
  gToRow1 :: f record -> [SQL.SQLData]

instance GToRow1 inner => GToRow1 (M1 cons meta inner) where
  gToRow1 (M1 inner) = gToRow1 inner

instance (GToRow1 left, GToRow1 right) => GToRow1 (left :*: right) where
  gToRow1 (left :*: right) = gToRow1 left <> gToRow1 right

instance (SQL.F.ToField inner) => GToRow1 (K1 meta inner) where
  gToRow1 (K1 inner) = [SQL.F.toField inner]

defaultToRow
  :: (Generic record, GToRow1 (Rep record))
  => record -> [SQL.SQLData]
defaultToRow = gToRow1 . from

class GToRow record where
  gToRow :: record -> [SQL.SQLData]
  default gToRow :: (Generic record, GToRow1 (Rep record)) => record -> [SQL.SQLData]
  gToRow = defaultToRow

newtype ToRowT a = ToRowT {unToRowT :: a}

instance (Generic a, GToRow1 (Rep a)) => To.ToRow (ToRowT a) where
  toRow = defaultToRow . unToRowT


class GFromRow1 f where
  gFromRow1 :: From.RowParser (f a)

instance GFromRow1 inner => GFromRow1 (M1 cons meta inner) where
  gFromRow1 = M1 <$> gFromRow1 @inner

instance (GFromRow1 left, GFromRow1 right) => GFromRow1 (left :*: right) where
  gFromRow1 = do
    l <- gFromRow1 @left
    r <- gFromRow1 @right
    pure (l :*: r)

instance (FromF.FromField inner) => GFromRow1 (K1 meta inner) where
  gFromRow1 = K1 <$> From.field

defaultGFromRow
  :: (Generic record, GFromRow1 (Rep record))
  => From.RowParser record
defaultGFromRow = to <$> gFromRow1

class GFromRow record where
  gFromRow :: From.RowParser record
  default gFromRow :: (Generic record, GFromRow1 (Rep record)) => From.RowParser record
  gFromRow = defaultGFromRow

newtype FromRowT a = FromRowT { unFromRowT :: a }

instance (Generic a, GFromRow1 (Rep a)) => From.FromRow (FromRowT a) where
  -- fromRow = FromRowT <$> defaultGFromRow
  fromRow = (FromRowT . to) <$> gFromRow1 @(Rep a)
