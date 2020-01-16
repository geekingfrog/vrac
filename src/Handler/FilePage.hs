{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- TODO fix that
{-# OPTIONS_GHC -Wwarn #-}

module Handler.FilePage where

import qualified DB.Class as DB
import qualified DB.FileMetadata as DB.FM
import qualified DB.Token as DB.Tok
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Import as Im
import System.FilePath ((</>))
import qualified Text.Blaze as Blaze
import qualified Text.Hamlet as H
import qualified Yesod.Core as Y.Co
import qualified Yesod.Form as Form
import qualified Data.Text as Tx
import qualified Data.Text.Encoding as Tx
import qualified Network.HTTP.Types.Status as Status
import qualified System.Posix as Sys
import Text.Printf (printf)

newtype UploadInfo
  = UploadInfo
      { fileInfo :: FileInfo
      }

uploadDirectory :: FilePath
uploadDirectory = "/tmp" </> "vrac"

vracLayout :: Widget -> Handler Html
vracLayout widget = do
  pc <- widgetToPageContent widget
  withUrlRenderer $(H.hamletFile "templates/vrac-layout.hamlet")

uploadForm :: Html -> Form.MForm Handler (Form.FormResult UploadInfo, Widget)
uploadForm =
  renderDivs $
    UploadInfo
      <$> fileAFormReq "Choose a file"

getFilePageR :: TokenPath -> Im.Handler Html
getFilePageR tokenPath = DB.getFilesMetadata (coerce tokenPath) >>= \case
  [] -> uploadHandler tokenPath
  files -> downloadHandler tokenPath files

uploadHandler :: TokenPath -> Im.Handler Html
uploadHandler tokenPath = withValidToken tokenPath $ \_token -> do
  (formWidget, enctype) <- Form.generateFormPost uploadForm
  vracLayout $ uploadWidget "Upload a file" tokenPath formWidget enctype

downloadHandler :: TokenPath -> [DB.FM.FileMetadata] -> Im.Handler Html
downloadHandler tokenPath files = case files of
  [] -> Y.Co.sendResponseStatus Status.status400 ("No files found for this url :/" :: Text)
  [f] -> do
    params <- Y.Co.reqGetParams <$> Y.Co.getRequest
    if "raw" `elem` (map fst params)
      then -- send raw file with correct content type & shit
        let contentType = Tx.encodeUtf8 $ DB.FM.contentType f
            fullPath = DB.FM.localPath f
        in $(logInfo) ("Sending file: " <> Tx.pack fullPath) *> Y.Co.sendFile contentType fullPath
      else do
        let fn = DB.FM.name f
        size <- liftIO $ getFileSize (DB.FM.localPath f)
        vracLayout $ do
          setTitle [shamlet|Download #{fn}|]
          toWidget
            [whamlet|
              <h2> Download and preview stuff there
              <p>
              <a href=@?{(FilePageR tokenPath, [("raw", "")])}>
                Open
              #{fn} - #{DB.FM.contentType f} (#{prettySize size} bytes)
            |]

  _ -> Y.Co.sendResponseStatus Status.status501 ("Multiple files not implemented" :: Text)

postFilePageR :: TokenPath -> Im.Handler Html
postFilePageR tokenPath = withValidToken tokenPath $ \token -> do
  ((formResult, formWidget), enctype) <- Form.runFormPost uploadForm
  case formResult of
    Form.FormSuccess (UploadInfo uploadInfo) -> do
      fId <- liftIO UUID.nextRandom
      let fn = Y.Co.fileName uploadInfo
      let saveLocation = uploadDirectory </> UUID.toString fId <> "-" <> unpack fn
      let metadata = DB.FM.FileMetadata
            { DB.FM.id = DB.FM.MetadataId fId,
              DB.FM.name = fn,
              DB.FM.contentType = Y.Co.fileContentType uploadInfo,
              DB.FM.localPath = saveLocation,
              DB.FM.tokenId = DB.Tok._tTokenId token
            }
      DB.saveFiles token [metadata]
      liftIO $ Y.Co.fileMove uploadInfo saveLocation
      downloadHandler tokenPath [metadata]
    _ -> vracLayout $ uploadWidget "Error uploading file" tokenPath formWidget enctype

withValidToken :: TokenPath -> (DB.Tok.Token -> Im.Handler Html) -> Im.Handler Html
withValidToken tokenPath handler =
  DB.getValidToken (coerce tokenPath) >>= \case
    Nothing -> renderNoToken
    Just tok -> handler tok
  where
    renderNoToken = vracLayout $ do
      setTitle "No valid token"
      toWidget
        [whamlet|
          <h2>No valid token found
        |]

uploadWidget ::
  (MonadWidget m, Blaze.ToMarkup a, ToWidget App b, HandlerSite m ~ App) =>
  Html ->
  TokenPath ->
  b ->
  a ->
  m ()
uploadWidget title tokenPath form enctype = do
  setTitle title
  toWidget
    [whamlet|
      <h2> Upload a file
      <form method=post action=@{FilePageR tokenPath} enctype=#{enctype}>
        ^{form}
        <button>Upload
    |]

getFileSize :: FilePath -> IO Word64
getFileSize fp = do
    stat <- Sys.getFileStatus fp
    let (Sys.COff s) = Sys.fileSize stat
    pure $ fromIntegral s

prettySize :: Word64 -> Text
prettySize size
  | s < 1024 = showTx s <> "b"
  | s < mb = Tx.pack $ printf "%.2fKb" (s / kb)
  | s < gb = Tx.pack $ printf "%.2fMb" (s / mb)
  | otherwise = Tx.pack $ printf "%.2fGb" (s / gb)

  where
    s, kb, mb, gb :: Double
    kb = 1024
    mb = 1024 * kb
    gb = 1024 * mb
    s = fromIntegral size
