module Web.Telegram.Types.Internal.API.EditMessageLiveLocation where

import Common
import Web.Telegram.Types.Internal.API.ChatId
import Web.Telegram.Types.Internal.ReplyMarkup

data EditMessageLiveLocation = EditMessageLiveLocation
  { chatId :: Maybe ChatId,
    messageId :: Maybe Int,
    inlineMessageId :: Maybe Text,
    latitude :: Float,
    longitude :: Float,
    replyMarkup :: Maybe ReplyMarkup
  }
  deriving stock (Show, Eq)

mkLabel ''EditMessageLiveLocation
deriveToJSON snake ''EditMessageLiveLocation
makeMethod ''EditMessageLiveLocation
