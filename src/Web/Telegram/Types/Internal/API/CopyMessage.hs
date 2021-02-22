module Web.Telegram.Types.Internal.API.CopyMessage where

import Common
import Web.Telegram.Types.Internal.API.ChatId
import Web.Telegram.Types.Internal.InputMedia
import Web.Telegram.Types.Internal.MessageEntity
import Web.Telegram.Types.Internal.ReplyMarkup

data CopyMessage = CopyMessage
  { chatId :: ChatId,
    fromChatId :: ChatId,
    messageId :: Int,
    caption :: Maybe Text,
    parseMode :: Maybe ParseMode,
    captionEntities :: Maybe [MessageEntity],
    disableNotification :: Maybe Bool,
    replyToMessageId :: Maybe Int,
    replyMarkup :: ReplyMarkup
  }
  deriving stock (Show, Eq)

mkLabel ''CopyMessage
deriveToJSON snake ''CopyMessage
