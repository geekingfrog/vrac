{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.FilePage where

import Import as Im
import qualified Text.Hamlet as H
import qualified Yesod.Form  as Form
import qualified Yesod.Core  as Y.Co
import System.FilePath ((</>))

newtype UploadInfo = UploadInfo
  { fileInfo :: FileInfo
  }

uploadDirectory :: FilePath
uploadDirectory = "/tmp" </> "vrac"

vracLayout :: Widget -> Handler Html
vracLayout widget = do
  pc <- widgetToPageContent widget
  withUrlRenderer $(H.hamletFile "templates/vrac-layout.hamlet")

uploadForm :: Html -> Form.MForm Handler (Form.FormResult UploadInfo, Widget)
uploadForm = renderDivs $ UploadInfo
  <$> fileAFormReq "Choose a file"

getFilePageR :: FilePath -> Im.Handler Html
getFilePageR filePath = do
  params <- Y.Co.reqGetParams <$> Y.Co.getRequest
  if "raw" `elem` map fst params
    then Y.Co.sendFile "image/jpeg" (uploadDirectory </> "foo")
    else do
      (formWidget, enctype) <- Form.generateFormPost uploadForm
      vracLayout $ do
        setTitle "Upload a file"
        toWidget
          [whamlet|
            <h2> Upload a file
            <form method=post action=@{FilePageR filePath} enctype=#{enctype}>
              ^{formWidget}
              <button>Upload

            <h2> query params:
            <ul>
              $forall (k, v) <- params
                <li>
                  <b>#{k}</b>: #{v}
          |]


postFilePageR :: FilePath -> Im.Handler Html
postFilePageR filePath = do
  ((formResult, _formWidget), enctype) <- Form.runFormPost uploadForm
  case formResult of
    Form.FormSuccess uploadInfo -> do
      let fn = Y.Co.fileName (fileInfo uploadInfo)
      let saveLocation = uploadDirectory </> filePath
      liftIO $ putStrLn $ "content type is: " <> Y.Co.fileContentType (fileInfo uploadInfo)
      -- liftIO $ putStrLn $ "enctype is: " <> showTx enctype
      liftIO $ Y.Co.fileMove (fileInfo uploadInfo) saveLocation
      vracLayout $ do
        setTitle [shamlet|#{fn}|]
        toWidget
          [whamlet|
            <h2>File uploaded: #{fn}
            <p>
              <a href=@?{(FilePageR filePath, [("raw", "")])}>Download
          |]
