module Web.Telegram.Types.Internal.API.SendLocation where

import Common
import Web.Telegram.Types.Internal.API.ChatId
import Web.Telegram.Types.Internal.ReplyMarkup

data SendLocation = SendLocation
  { chatId :: ChatId,
    latitude :: Float,
    longitude :: Float,
    livePeriod :: Maybe Int,
    disableNotification :: Maybe Bool,
    replyToMessageId :: Maybe Int,
    replyMarkup :: Maybe ReplyMarkup
  }
  deriving stock (Show, Eq)

mkLabel ''SendLocation
deriveToJSON snake ''SendLocation
