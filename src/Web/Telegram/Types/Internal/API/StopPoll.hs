module Web.Telegram.Types.Internal.API.StopPoll where

import Common
import Web.Telegram.Types.Internal.API.ChatId
import Web.Telegram.Types.Internal.InlineKeyboardMarkup

data StopPoll = StopPoll
  { chatId :: ChatId,
    messageId :: Int,
    replyMarkup :: Maybe InlineKeyboardMarkup
  }
  deriving stock (Show, Eq)

mkLabel ''StopPoll
deriveToJSON snake ''StopPoll
makeMethod ''StopPoll
