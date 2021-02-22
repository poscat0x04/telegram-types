module Web.Telegram.Types.Internal.API.SetChatTitle where

import Common
import Web.Telegram.Types.Internal.API.ChatId

data SetChatTitle = SetChatTitle
  { chatId :: ChatId,
    title :: Text
  }
  deriving stock (Show, Eq)

mkLabel ''SetChatTitle
deriveToJSON snake ''SetChatTitle
