{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module DB.Class
  ( MonadPersistToken (..),
    PersistTokenT (..),
    ValidTokenAlreadyExist (..),
  )
where

import qualified Control.Exception.Safe as Exc
import qualified DB.Token as Tok
import qualified Data.Text as Tx
import qualified Database.SQLite.Simple as SQL
import Import.NoFoundation

class MonadPersistToken m where
  persistToken :: Tok.TokenInsert -> m Tok.Token
  getValidToken :: Tok.TokenName -> m (Maybe Tok.Token)

newtype PersistTokenT m a = PersistTokenT {unPersistTokenT :: m a}
  deriving newtype (Functor, Applicative, Monad)

newtype ValidTokenAlreadyExist = ValidTokenAlreadyExist Tok.TokenName
  deriving (Show, Typeable)

instance Exception ValidTokenAlreadyExist

instance (MonadIO m, MonadLogger m) => MonadPersistToken (PersistTokenT m) where
  persistToken :: Tok.TokenInsert -> PersistTokenT m Tok.Token
  persistToken tok = PersistTokenT $ do
    conn <- liftIO $ SQL.open "vrac.sqlite"
    $(logInfo) $ "Persisting token: " <> showTx tok
    let tokName = Tok._tiName tok
    liftIO $ SQL.withTransaction conn $ do
      (exists :: [SQL.Only Text]) <-
        SQL.query
          conn
          "SELECT name from token where name = ? AND is_valid = true"
          (SQL.Only tokName)
      case exists of
        [] -> do
          SQL.execute conn "INSERT INTO token (name, max_size, expires_at) VALUES (?,?,?)" tok
          putStrLn "token persisted"
          liftIO (getValidToken' conn tokName) >>= \case
            Just t -> pure t
            Nothing -> Exc.throwString $ Tx.unpack $ "No valid token found with name " <> (Tok.getTokenName tokName)
        _ -> Exc.throwIO $ ValidTokenAlreadyExist tokName
  {-# INLINE persistToken #-}

  getValidToken :: MonadIO m => Tok.TokenName -> PersistTokenT m (Maybe Tok.Token)
  getValidToken tokName = PersistTokenT $ liftIO do
    conn <- SQL.open "vrac.sqlite"
    getValidToken' conn tokName
  {-# INLINE getValidToken #-}

getValidToken' :: SQL.Connection -> Tok.TokenName -> IO (Maybe Tok.Token)
getValidToken' conn tokName = do
  r <-
    SQL.query
      conn
      "SELECT id, name, max_size, expires_at, is_valid, created_at FROM token WHERE name = ? AND is_valid = true"
    (SQL.Only tokName)
  case r of
    [tok] -> pure (Just tok)
    [] -> pure Nothing
    _ -> Exc.throwString $ Tx.unpack $ "Found multiple valid token with name " <> (Tok.getTokenName tokName)
