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
import qualified DB.FileMetadata as FM
import qualified Data.Text as Tx
import qualified Database.SQLite.Simple as SQL
import Import.NoFoundation

class MonadPersistToken m where
  persistToken :: Tok.TokenInsert -> m Tok.Token
  getValidToken :: Tok.TokenName -> m (Maybe Tok.Token)
  saveFiles :: Tok.Token -> [FM.FileMetadata] -> m ()
  getFilesMetadata :: Tok.TokenName -> m [FM.FileMetadata]

newtype PersistTokenT m a = PersistTokenT {unPersistTokenT :: m a}
  deriving newtype (Functor, Applicative, Monad)

newtype ValidTokenAlreadyExist = ValidTokenAlreadyExist Tok.TokenName
  deriving (Show, Typeable)

instance Exception ValidTokenAlreadyExist

instance (MonadIO m, MonadLogger m, MonadThrow m) => MonadPersistToken (PersistTokenT m) where
  persistToken :: Tok.TokenInsert -> PersistTokenT m Tok.Token
  persistToken tok = PersistTokenT $ do
    conn <- liftIO $ SQL.open "vrac.sqlite"
    $(logInfo) $ "Persisting token: " <> showTx tok
    let tokName = Tok._tiName tok
    liftIO $ SQL.withTransaction conn $ do
      (exists :: [SQL.Only Text]) <-
        SQL.query
          conn
          "SELECT name from token where name = ? AND is_valid = true AND expires_at > datetime('now')"
          (SQL.Only tokName)
      case exists of
        [] -> do
          SQL.execute conn "INSERT INTO token (name, max_size, expires_at) VALUES (?,?,?)" tok
          putStrLn "token persisted"
        _ -> Exc.throwIO $ ValidTokenAlreadyExist tokName
    liftIO (getValidToken' conn tokName) >>= \case
      Just t -> pure t
      Nothing -> Exc.throwString $ Tx.unpack $ "No valid token found with name " <> (Tok.getTokenName tokName)

  getValidToken :: MonadIO m => Tok.TokenName -> PersistTokenT m (Maybe Tok.Token)
  getValidToken tokName = PersistTokenT $ liftIO do
    conn <- SQL.open "vrac.sqlite"
    getValidToken' conn tokName

  saveFiles :: MonadIO m => Tok.Token -> [FM.FileMetadata] -> PersistTokenT m ()
  saveFiles token files = PersistTokenT $ liftIO $ SQL.withConnection "vrac.sqlite" $ \conn -> do
    SQL.withTransaction conn $ do
      let tokId = Tok._tTokenId token
      SQL.executeMany
        conn
        "INSERT INTO file_metadata (id, file_name, file_content_type, file_local_path, token_id) VALUES (?, ?, ?, ?, ?)"
        files

      SQL.execute
        conn
        "UPDATE token SET is_valid = false WHERE id = ?"
        (SQL.Only tokId)

  getFilesMetadata :: MonadIO m => Tok.TokenName -> PersistTokenT m [FM.FileMetadata]
  getFilesMetadata tokenName = PersistTokenT $ liftIO $ SQL.withConnection "vrac.sqlite" $ \conn -> do
    SQL.query
      conn
      "SELECT F.id, F.file_name, F.file_content_type, F.file_local_path, F.token_id from file_metadata AS F INNER JOIN token as T ON T.id = F.token_id WHERE T.deleted_at IS NULL AND T.name = ?"
      (SQL.Only tokenName)

getValidToken' :: SQL.Connection -> Tok.TokenName -> IO (Maybe Tok.Token)
getValidToken' conn tokName = do
  r <-
    SQL.query
      conn
      "SELECT id, name, max_size, expires_at, is_valid, created_at, deleted_at FROM token WHERE name = ? AND is_valid = true AND expires_at > datetime('now')"
    (SQL.Only tokName)
  case r of
    [tok] -> pure (Just tok)
    [] -> pure Nothing
    _ -> Exc.throwString $ Tx.unpack $ "Found multiple valid token with name " <> (Tok.getTokenName tokName)
