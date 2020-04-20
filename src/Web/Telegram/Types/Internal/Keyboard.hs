{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Web.Telegram.Types.Internal.Keyboard where

import Data.Aeson
import Data.Aeson.Types
import Data.Hashable
import Data.Text (Text)
import Deriving.Aeson
import Servant.API
import Web.Telegram.Types.Internal.Media
import Web.Telegram.Types.Internal.Utils

data ReplyKeyboardMarkup
  = ReplyKeyboardMarkup
      { keyboard :: [[KeyboardButton]],
        resizeKeyboard :: Maybe Bool,
        oneTimeKeyboard :: Maybe Bool,
        selective :: Maybe Bool
      }
  deriving (Show, Eq, Generic, Default, Hashable)
  deriving
    (ToJSON)
    via Snake ReplyKeyboardMarkup
  deriving (ToHttpApiData) via Serialize ReplyKeyboardMarkup

data KeyboardButton
  = KeyboardButton
      { text :: Text,
        addon :: Maybe KeyboardButtonAddon
      }
  deriving (Show, Eq, Generic, Default, Hashable)
  deriving (ToHttpApiData) via Serialize KeyboardButton

instance ToJSON KeyboardButton where
  toJSON KeyboardButton {..} =
    let Object t = object ["text" .= text]
        Object a = maybe emptyObject toJSON addon
     in Object $ t <> a

data KeyboardButtonAddon
  = RequestContact Bool
  | RequestLocation Bool
  | RequestPoll KeyboardButtonPollType
  deriving (Show, Eq, Generic, Default, Hashable)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[ConstructorTagModifier CamelToSnake, SumObjectWithSingleField] KeyboardButtonAddon

newtype KeyboardButtonPollType
  = KeyboardButtonPollType
      { pollType :: Maybe PollType
      }
  deriving (Show, Eq, Generic, Default, Hashable)
  deriving
    (FromJSON, ToJSON)
    via PrefixedSnake "poll" KeyboardButtonPollType
  deriving (ToHttpApiData) via Serialize KeyboardButtonPollType

data ReplyKeyboardRemove
  = ReplyKeyboardRemove
      { removeKeyboard :: Bool,
        selective :: Maybe Bool
      }
  deriving (Show, Eq, Generic, Default, Hashable)
  deriving
    (FromJSON, ToJSON)
    via Snake ReplyKeyboardRemove
  deriving (ToHttpApiData) via Serialize ReplyKeyboardRemove

newtype InlineKeyboardMarkup
  = InlineKeyboardMarkup
      { inlineKeyboard :: [[InlineKeyboardButton]]
      }
  deriving (Show, Eq, Generic, Default, Hashable)
  deriving
    (ToJSON, FromJSON)
    via Snake InlineKeyboardMarkup
  deriving (ToHttpApiData) via Serialize InlineKeyboardMarkup

data InlineKeyboardButton
  = InlineKeyboardButton
      { text :: Text,
        url :: Maybe Text,
        loginUrl :: Maybe LoginUrl,
        callbackData :: Maybe Text,
        switchInlineQuery :: Maybe Text,
        switchInlineQueryCurrentChat :: Maybe Text,
        pay :: Maybe Bool
      }
  deriving (Show, Eq, Generic, Default, Hashable)
  deriving
    (FromJSON, ToJSON)
    via Snake InlineKeyboardButton
  deriving (ToHttpApiData) via Serialize InlineKeyboardButton

data LoginUrl
  = LoginUrl
      { url :: Text,
        forwardText :: Maybe Text,
        botUsername :: Maybe Text,
        requestWriteAccess :: Maybe Bool
      }
  deriving (Show, Eq, Generic, Default, Hashable)
  deriving
    (FromJSON, ToJSON)
    via Snake LoginUrl
  deriving (ToHttpApiData) via Serialize LoginUrl

data ForceReply
  = ForceReply
      { forceReply :: Bool,
        selective :: Maybe Bool
      }
  deriving (Show, Eq, Generic, Default, Hashable)
  deriving
    (FromJSON, ToJSON)
    via Snake ForceReply
  deriving (ToHttpApiData) via Serialize ForceReply
