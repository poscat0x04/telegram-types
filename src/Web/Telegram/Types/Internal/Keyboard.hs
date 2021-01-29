{-# LANGUAGE StandaloneDeriving #-}

module Web.Telegram.Types.Internal.Keyboard where

import Common
import Web.Telegram.Types.Internal.Poll

data ReplyKeyboardMarkup = ReplyKeyboardMarkup
  { keyboard :: [[KeyboardButton]],
    resizeKeyboard :: Maybe Bool,
    oneTimeKeyboard :: Maybe Bool,
    selective :: Maybe Bool
  }
  deriving stock (Show, Eq)

data KeyboardButton = KeyboardButton
  { text :: NoFlatten Text,
    addon :: Maybe KeyboardButtonAddon
  }
  deriving stock (Show, Eq, Generic)

deriving via Flatten KeyboardButton instance FromJSON KeyboardButton

deriving via Flatten KeyboardButton instance ToJSON KeyboardButton

data KeyboardButtonAddon
  = RequestContact Bool
  | RequestLocation Bool
  | RequestPoll KeyboardButtonPollType
  deriving stock (Show, Eq)

newtype KeyboardButtonPollType = KeyboardButtonPollType
  { _type :: Maybe PollType
  }
  deriving stock (Show, Eq)

data ReplyKeyboardRemove = ReplyKeyboardRemove
  { removeKeyboard :: Bool,
    selective :: Maybe Bool
  }
  deriving stock (Show, Eq)

newtype InlineKeyboardMarkup = InlineKeyboardMarkup
  { inlineKeyboard :: [[InlineKeyboardButton]]
  }
  deriving stock (Show, Eq)

data InlineKeyboardButton = InlineKeyboardButton
  { text :: Text,
    url :: Maybe Text,
    loginUrl :: Maybe LoginUrl,
    callbackData :: Maybe Text,
    switchInlineQuery :: Maybe Text,
    switchInlineQueryCurrentChat :: Maybe Text,
    pay :: Maybe Bool
  }
  deriving stock (Show, Eq)

data LoginUrl = LoginUrl
  { url :: Text,
    forwardText :: Maybe Text,
    botUsername :: Maybe Text,
    requestWriteAccess :: Maybe Bool
  }
  deriving stock (Show, Eq)

mkLabel ''ReplyKeyboardMarkup
makePrismLabels ''KeyboardButtonAddon
mkLabel ''KeyboardButtonPollType
mkLabel ''ReplyKeyboardRemove
mkLabel ''InlineKeyboardMarkup
mkLabel ''InlineKeyboardButton
mkLabel ''LoginUrl
deriveJSON snake ''ReplyKeyboardMarkup
deriveJSON (defaultOptions {sumEncoding = ObjectWithSingleField, constructorTagModifier = camelTo2 '_'}) ''KeyboardButtonAddon
deriveJSON snake ''KeyboardButtonPollType
deriveJSON snake ''ReplyKeyboardRemove
deriveJSON snake ''InlineKeyboardMarkup
deriveJSON snake ''InlineKeyboardButton
deriveJSON snake ''LoginUrl
