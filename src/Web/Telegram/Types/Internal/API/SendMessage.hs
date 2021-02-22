module Web.Telegram.Types.Internal.API.SendMessage where

import Common
import Web.Telegram.Types.Internal.API.ChatId
import Web.Telegram.Types.Internal.InputMedia
import Web.Telegram.Types.Internal.MessageEntity
import Web.Telegram.Types.Internal.ReplyMarkup

data SendMessage = SendMessage
  { chatId :: ChatId,
    text :: Text,
    parseMode :: Maybe ParseMode,
    entities :: Maybe [MessageEntity],
    disableWebPageView :: Maybe Bool,
    disableNotification :: Maybe Bool,
    replyToMessageId :: Maybe Int,
    allowSendingWithoutReply :: Maybe Bool,
    replyMarkup :: ReplyMarkup
  }
  deriving stock (Show, Eq)

mkLabel ''SendMessage
deriveToJSON snake ''SendMessage
