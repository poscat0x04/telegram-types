module Web.Telegram.Types.Internal.API.StopMessageLiveLocation where

import Common
import Web.Telegram.Types.Internal.API.ChatId
import Web.Telegram.Types.Internal.ReplyMarkup

data StopMessageLiveLocation = StopMessageLiveLocation
  { chatId :: Maybe ChatId,
    messageId :: Maybe Int,
    inlineMessageId :: Maybe Text,
    replyMarkup :: Maybe ReplyMarkup
  }
  deriving stock (Show, Eq)

mkLabel ''StopMessageLiveLocation
deriveToJSON snake ''StopMessageLiveLocation
makeMethod ''StopMessageLiveLocation
