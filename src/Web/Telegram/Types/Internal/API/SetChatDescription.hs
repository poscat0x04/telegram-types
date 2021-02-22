module Web.Telegram.Types.Internal.API.SetChatDescription where

import Common
import Web.Telegram.Types.Internal.API.ChatId

data SetChatDescription = SetChatDescription
  { chatId :: ChatId,
    description :: Text
  }
  deriving stock (Show, Eq)

mkLabel ''SetChatDescription
deriveToJSON snake ''SetChatDescription
