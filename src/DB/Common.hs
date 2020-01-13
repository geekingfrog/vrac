{-# LANGUAGE DerivingVia #-}

module DB.Common where

import GHC.Word
import qualified Database.SQLite.Simple.ToField as F

newtype SizeInByte = SizeInByte { getSizeInByte :: Word64 }
  deriving stock (Show)
  deriving F.ToField via Word64
