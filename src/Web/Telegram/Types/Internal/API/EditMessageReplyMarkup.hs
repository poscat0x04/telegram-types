module Web.Telegram.Types.Internal.API.EditMessageReplyMarkup where

import Common
import Web.Telegram.Types.Internal.API.ChatId
import Web.Telegram.Types.Internal.InlineKeyboardMarkup

data EditMessageReplyMarkup = EditMessageReplyMarkup
  { chatId :: Maybe ChatId,
    messageId :: Maybe Int,
    inlineMessageId :: Maybe Text,
    replyMarkup :: Maybe InlineKeyboardMarkup
  }
  deriving stock (Show, Eq)

mkLabel ''EditMessageReplyMarkup
deriveToJSON snake ''EditMessageReplyMarkup
