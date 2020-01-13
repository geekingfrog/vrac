{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.GenToken where

import Import hiding (expiresAt)
import qualified Text.Hamlet as H
import qualified Yesod.Form  as Form
import qualified Yesod.Core  as Y.Co
import qualified Data.Time.Clock as T
import qualified DB.Class as DB
import qualified DB.Token as DB.Tok
-- import qualified Control.Exception.Safe as Exc
import qualified Control.Exception as Exc

import qualified Data.Text as Tx

data Token = Token
  { name :: Text
  , maxSize :: MaxSize
  , expiresAt :: Expiration
  }
  deriving stock (Eq)

data MaxSize
  = SizeLimited SizeInByte
  | Unlimited
  deriving stock (Eq)

data Expiration
  = ExpiresIn T.NominalDiffTime
  | DoesntExpire
  deriving stock (Eq)

newtype SizeInByte = SizeInByte { getSizeInByte :: Word64 }
  deriving newtype (Eq)

toDBO :: T.UTCTime -> Token -> DB.Tok.TokenInsert
toDBO now tok = DB.Tok.TokenInsert
  { DB.Tok._tiName = DB.Tok.TokenName (name tok)
  , DB.Tok._tiMaxSize = case maxSize tok of
      SizeLimited lim -> Just $ coerce lim
      Unlimited -> Nothing
  , DB.Tok._tiExpiresAt = case expiresAt tok of
      ExpiresIn diff -> Just $ T.addUTCTime diff now
      DoesntExpire -> Nothing
  }

tokenForm :: Html -> Form.MForm Handler (Form.FormResult Token, Widget)
tokenForm = renderDivs $ Token
  <$> Form.areq Form.textField "name" (Just "foo")
  <*> (Form.areq
    (Form.radioField (Form.optionsPairs sizePairs))
    "Maximum size" (Just $ SizeLimited $ SizeInByte $ 2 * mB))
  <*> (Form.areq
    (Form.radioField (Form.optionsPairs expirationPairs))
    "Expires after" (Just $ ExpiresIn 3600))

  where
    sizePairs :: [(Text, MaxSize)]
    sizePairs =
      [ ("2 MB", SizeLimited $ SizeInByte $ 2 * mB)
      , ("200 MB", SizeLimited $ SizeInByte $ 200 * mB)
      , ("2 GB", SizeLimited $ SizeInByte $ 2 * gB)
      , ("Unlimited", Unlimited)
      ]
    mB = 1024 * 1024
    gB = 1024 * mB

    expirationPairs :: [(Text, Expiration)]
    expirationPairs =
      [ ("1 hour", ExpiresIn 3600)
      , ("1 day", ExpiresIn $ 3600 * 24)
      , ("1 week", ExpiresIn $ 3600 * 24 * 7)
      , ("doesn't expire", DoesntExpire)
      ]

vracLayout :: Widget -> Handler Html
vracLayout widget = do
  pc <- widgetToPageContent widget
  withUrlRenderer $(H.hamletFile "templates/vrac-layout.hamlet")


getGenTokenR :: Handler Html
getGenTokenR = do
  (formWidget, enctype) <- Form.generateFormPost tokenForm
  vracLayout $ do
    setTitle "Generate a token"
    toWidget
      [whamlet|
        <h2> Generate a token
        <form method=post action=@{GenTokenR} enctype=#{enctype}>
          ^{formWidget}
          <button>Generate
      |]

postGenTokenR :: Handler Html
postGenTokenR = do
  ((result, formWidget), enctype) <- Form.runFormPost tokenForm
  case result of
    Form.FormSuccess token -> do
      now <- liftIO T.getCurrentTime
      try (DB.persistToken (toDBO now token)) >>= \case
        Right _ -> Y.Co.redirect (FilePageR (Tx.unpack (name token)))
        Left (_ :: DB.ValidTokenAlreadyExist) ->
          reRender formWidget enctype (Just $ "A valid token with name " <> (name token) <> " already exist")
        Left otherExc -> liftIO (Exc.throwIO otherExc)
    _ -> reRender formWidget enctype Nothing

  where
    reRender formWidget enctype (mbErrorMsg :: Maybe Text) = vracLayout do
      setTitle "Generate a token"
      toWidget
        [whamlet|
          <h2> Generate a token
          $maybe err <- mbErrorMsg
            <div .error>
              #{err}
          <form method=post action=@{GenTokenR} enctype=#{enctype}>
            ^{formWidget}
            <button>Generate
        |]
