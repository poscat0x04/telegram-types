module Web.Telegram.Types.Internal.API.SendSticker where

import Common
import Web.Telegram.Types.Internal.API.ChatId
import Web.Telegram.Types.Internal.ReplyMarkup

data SendSticker = SendSticker
  { chatId :: ChatId,
    sticker :: Text,
    disableNotification :: Maybe Bool,
    replyToMessageId :: Maybe Int,
    allowSendingWithoutReply :: Maybe Bool,
    replyMarkup :: Maybe ReplyMarkup
  }
  deriving stock (Show, Eq)

mkLabel ''SendSticker
deriveToJSON snake ''SendSticker
