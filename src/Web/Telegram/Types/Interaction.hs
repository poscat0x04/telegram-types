{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | User interactions: customized keyboards, clickable buttons, popups and inline displays
module Web.Telegram.Types.Interaction
  ( ReplyKeyboardMarkup (..),
    KeyboardButton (..),
    KeyboardButtonPollType (..),
    ReplyKeyboardRemove (..),
    InlineKeyboardMarkup (..),
    ForceReply (..),
    LoginUrl (..),
    ReplyMarkup,
  )
where

import Data.Aeson
import Data.OpenUnion
import Deriving.Aeson
import Servant.API
import Web.Telegram.Types.Internal.Keyboard
import Web.Telegram.Types.Internal.Utils

type ReplyMarkup =
  Union
    '[ InlineKeyboardMarkup,
       ReplyKeyboardMarkup,
       ReplyKeyboardRemove,
       ForceReply
     ]

instance ToJSON ReplyMarkup where
  toJSON =
    (\(inlineM :: InlineKeyboardMarkup) -> toJSON inlineM)
      @> (\(replyM :: ReplyKeyboardMarkup) -> toJSON replyM)
      @> (\(replyR :: ReplyKeyboardRemove) -> toJSON replyR)
      @> (\(forceR :: ForceReply) -> toJSON forceR)
      @> typesExhausted

deriving via Serialize ReplyMarkup instance ToHttpApiData ReplyMarkup

data ChatAction
  = Typing
  | UploadPhoto
  | RecordVideo
  | UploadVideo
  | RecordAudio
  | UploadAudio
  | UploadDocument
  | FindLocation
  | RecordVideoNote
  | UploadVideoNote
  deriving (Show, Eq, Ord, Generic, Default)
  deriving (ToJSON, FromJSON) via Snake ChatAction
  deriving (ToHttpApiData) via Serialize ChatAction
