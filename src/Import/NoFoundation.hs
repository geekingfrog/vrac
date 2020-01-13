{-# LANGUAGE CPP #-}
module Import.NoFoundation
    ( module Import
    , identity
    , showTx
    , coerce
    ) where

import ClassyPrelude.Yesod   as Import hiding (id)
import Settings              as Import
import Settings.StaticFiles  as Import
import Yesod.Core.Types      as Import (loggerSet)
import Yesod.Default.Config2 as Import

import qualified ClassyPrelude.Yesod as CPY
import qualified Data.Text as Tx
import Data.Coerce (coerce)

identity :: a -> a
identity = CPY.id

showTx :: Show a => a -> Text
showTx = Tx.pack Prelude.. show
