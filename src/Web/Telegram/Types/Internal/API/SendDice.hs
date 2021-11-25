module Web.Telegram.Types.Internal.API.SendDice where

import Common
import Web.Telegram.Types.Internal.API.ChatId
import Web.Telegram.Types.Internal.ReplyMarkup

data SendDice = SendDice
  { chatId :: ChatId,
    emoji :: Maybe Text,
    disableNotification :: Maybe Bool,
    replyToMessageId :: Maybe Int,
    allowSendingWithoutReply :: Maybe Bool,
    replyMarkup :: Maybe ReplyMarkup
  }
  deriving stock (Show, Eq)

mkLabel ''SendDice
deriveToJSON snake ''SendDice
makeMethod ''SendDice
