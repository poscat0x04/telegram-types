{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Web.Telegram.Types.Internal.Keyboard where

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
    (FromJSON, ToJSON)
    via Snake ReplyKeyboardMarkup
  deriving (ToHttpApiData) via Serialize ReplyKeyboardMarkup

data KeyboardButton
  = KeyboardButton
      { text :: Text,
        requestContact :: Maybe Bool,
        requestLocation :: Maybe Bool,
        requestPoll :: Maybe KeyboardButtonPollType
      }
  deriving (Show, Eq, Generic, Default, Hashable)
  deriving
    (FromJSON, ToJSON)
    via Snake KeyboardButton
  deriving (ToHttpApiData) via Serialize KeyboardButton

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
