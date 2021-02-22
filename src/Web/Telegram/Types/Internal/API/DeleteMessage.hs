module Web.Telegram.Types.Internal.API.DeleteMessage where

import Common
import Web.Telegram.Types.Internal.API.ChatId

data DeleteMessage = DeleteMessage
  { chatId :: ChatId,
    messageId :: Int
  }
  deriving stock (Show, Eq)

mkLabel ''DeleteMessage
deriveToJSON snake ''DeleteMessage
