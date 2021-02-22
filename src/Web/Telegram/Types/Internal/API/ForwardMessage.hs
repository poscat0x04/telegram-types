module Web.Telegram.Types.Internal.API.ForwardMessage where

import Common
import Web.Telegram.Types.Internal.API.ChatId

data ForwardMessage = ForwardMessage
  { chatId :: ChatId,
    fromChatId :: ChatId,
    disableNotification :: Maybe Bool,
    messageId :: Int
  }
  deriving stock (Show, Eq)

mkLabel ''ForwardMessage
deriveToJSON snake ''ForwardMessage
