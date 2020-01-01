{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.GenUrl where

import Import

import qualified Data.Text   as Tx
import qualified Text.Hamlet as H
import qualified Yesod.Form  as Form

import qualified Database.SQLite.Simple as SQL
import qualified DB.FileMetadata as DB.File

import qualified DB.Token

class PrettyShow a where
  prettyShow :: a -> String

testLayout :: Widget -> Handler Html
testLayout widget = do
  pc <- widgetToPageContent widget
  withUrlRenderer $(H.hamletFile "templates/test-layout.hamlet")

data UploadSettings = UploadSettings
  { customPath  :: Text
  , testI       :: Int
  , fileInfo    :: FileInfo
  }

instance Show UploadSettings where
  show us
    = "path: " <> Tx.unpack (customPath us)
    <> " testI: " <> show (testI us)
    <> " file <...>"

instance PrettyShow UploadSettings where
  prettyShow us
    = "path: " <> Tx.unpack (customPath us)
    <> " testI: " <> show (testI us)
    <> " filename: " <> Tx.unpack (fileName (fileInfo us))

uploadSettingsForm :: Html -> Form.MForm Handler (Form.FormResult UploadSettings, Widget)
uploadSettingsForm = renderDivs $ UploadSettings
  <$> areq textField "path" Nothing
  <*> areq intField "testing number" Nothing
  <*> fileAFormReq "Choose a file"


getGenUrlR :: Handler Html
getGenUrlR = do
  headerClass <- newIdent
  (formWidget, enctype) <- Form.generateFormPost uploadSettingsForm
  dbConn <- appDbConn <$> getYesod
  let fm = DB.File.FileMetadata
        { DB.File.id = 1
        , DB.File.name = "test_name"
        , DB.File.contentType = "content_type"
        , DB.File.size = DB.File.SizeInByte 1024
        , DB.File.localPath = "local/path"
        }
  -- liftIO $ SQL.execute
  --   dbConn
  --   "INSERT INTO file_metadata (id, file_name, file_content_type, file_size, file_local_path) VALUES (?, ?, ?, ?, ?)"
  --   fm
  testLayout $ do
    setTitle "gen url"
    toWidget
      [whamlet|
        <h2 .#{headerClass}>Coucou
        <form method=post action=@{GenUrlR} enctype=#{enctype}>
          ^{formWidget}
          <button>Submit
        |]

postGenUrlR :: Handler Html
postGenUrlR = do
  ((result, formWidget), enctype) <- Form.runFormPost uploadSettingsForm
  case result of
    Form.FormSuccess uploadSettings -> testLayout $ do
      setTitle "gen url posted"
      toWidget
        [whamlet|<p>#{prettyShow uploadSettings}|]

    x -> testLayout $ do
      putStrLn "didn't get a FormSuccess"
      print x
      setTitle "gen url with errors"
      headerClass <- newIdent
      toWidget
        [whamlet|
          <h2 .#{headerClass}>Coucou
          <form method=post action=@{GenUrlR} enctype=#{enctype}>
            ^{formWidget}
            <button>Submit
          |]
